!REAL:MODEL_LAYER:INITIALIZATION

#ifndef VERT_UNIT
!  This MODULE holds the routines which are used to perform various initializations
!  for the individual domains, specifically for the Eulerian, mass-based coordinate.

!-----------------------------------------------------------------------

MODULE module_initialize_real

   USE module_bc
   USE module_configure
   USE module_domain
   USE module_io_domain
   USE module_model_constants
   USE module_state_description
   USE module_timing
   USE module_soil_pre
   USE module_date_time
   USE module_llxy
   USE module_polarfft
#ifdef DM_PARALLEL
   USE module_dm
   USE module_comm_dm, ONLY : &
                           HALO_EM_INIT_1_sub   &
                          ,HALO_EM_INIT_2_sub   &
                          ,HALO_EM_INIT_3_sub   &
                          ,HALO_EM_INIT_4_sub   &
                          ,HALO_EM_INIT_5_sub   &
                          ,HALO_EM_INIT_6_sub   &
                          ,HALO_EM_VINTERP_UV_1_sub
#endif

   REAL , SAVE :: p_top_save
   INTEGER :: internal_time_loop

CONTAINS

!-------------------------------------------------------------------

   SUBROUTINE init_domain ( grid )

      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

      !  Local data.

      INTEGER :: idum1, idum2

      CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

        CALL init_domain_rk( grid &
!
#include "actual_new_args.inc"
!
      )
   END SUBROUTINE init_domain

!-------------------------------------------------------------------

   SUBROUTINE init_domain_rk ( grid &
!
#include "dummy_new_args.inc"
!
   )

      USE module_optional_input
      IMPLICIT NONE

      !  Input space and data.  No gridded meteorological data has been stored, though.

!     TYPE (domain), POINTER :: grid
      TYPE (domain)          :: grid

#include "dummy_new_decl.inc"

      TYPE (grid_config_rec_type)              :: config_flags

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat
      INTEGER :: loop , num_seaice_changes

      INTEGER :: ids, ide, jds, jde, kds, kde, &
                 ims, ime, jms, jme, kms, kme, &
                 its, ite, jts, jte, kts, kte, &
                 ips, ipe, jps, jpe, kps, kpe, &
                 i, j, k, kk

      INTEGER :: imsx, imex, jmsx, jmex, kmsx, kmex,    &
                 ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                 imsy, imey, jmsy, jmey, kmsy, kmey,    &
                 ipsy, ipey, jpsy, jpey, kpsy, kpey

      INTEGER :: ns

      !  Local data

      INTEGER :: error
      INTEGER :: im, num_3d_m, num_3d_s
      REAL    :: B1, B2, B3, B4, B5
      REAL    :: p_surf, p_level
      REAL    :: cof1, cof2
      REAL    :: qvf , qvf1 , qvf2 , qtot, pd_surf
      REAL    :: p00 , t00 , a , tiso, p_strat, a_strat
      REAL    :: hold_znw , ptemp
      REAL    :: vap_pres_mb , sat_vap_pres_mb
      LOGICAL :: were_bad

      LOGICAL :: stretch_grid, dry_sounding, debug
      INTEGER :: IICOUNT, icount

      REAL :: p_top_requested , temp
      INTEGER :: num_metgrid_levels
      REAL , DIMENSION(max_eta) :: eta_levels

      INTEGER :: auto_levels_opt
      REAL :: max_dz, dzbot, dzstretch_s, dzstretch_u, z1, airmass

      REAL :: dclat

!      INTEGER , PARAMETER :: nl_max = 1000
!      REAL , DIMENSION(nl_max) :: grid%dn

integer::oops1,oops2

      REAL    :: zap_close_levels
      INTEGER :: force_sfc_in_vinterp
      INTEGER :: interp_type , lagrange_order , extrap_type , t_extrap_type
      INTEGER :: linear_interp
      LOGICAL :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      LOGICAL :: we_have_tavgsfc , we_have_tsk

      INTEGER :: lev500 , loop_count
      REAL    :: zl , zu , pl , pu , z500 , dz500 , tvsfc , dpmu
      REAL    :: pfu, pfd, phm

      LOGICAL , PARAMETER :: want_full_levels = .TRUE.
      LOGICAL , PARAMETER :: want_half_levels = .FALSE.

      CHARACTER (LEN=256) :: a_message, mminlu
      REAL :: max_mf
    
      !  Excluded middle.

      LOGICAL :: any_valid_points
      INTEGER :: i_valid , j_valid
      
      !  Vert interpolation in WRF

      INTEGER :: k_max_p , k_min_p

!-- Carsel and Parrish [1988]
        REAL , DIMENSION(100) :: lqmi

      REAL , DIMENSION(100) :: thickness , levels
      REAL :: t_start , t_end
      REAL , ALLOCATABLE , DIMENSION(:,:) :: clat_glob

      ! added for multiple specified sets of eta_levels with vertical grid nesting
      INTEGER :: ks, ke, id
      LOGICAL :: vnest !T if using vertical nesting with vet_refine_method=2, otherwise F

      INTEGER :: j_save
      INTEGER :: change_soil, change_soilw, iforce

      REAL:: temp_rho

      LOGICAL :: wif_upside_down = .FALSE.
      
      !  Test on consistency between namelist settings and the available data from geogrid.

      INTEGER :: geogrid_flag_error

      !  Vertical pressure checks

      REAL :: press_above, press_below

      !  Dimension information stored in grid data structure.

      CALL cpu_time(t_start)
      CALL get_ijk_from_grid (  grid ,                   &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe,    &
                                imsx, imex, jmsx, jmex, kmsx, kmex,    &
                                ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                                imsy, imey, jmsy, jmey, kmsy, kmey,    &
                                ipsy, ipey, jpsy, jpey, kpsy, kpey )
      its = ips ; ite = ipe ; jts = jps ; jte = jpe ; kts = kps ; kte = kpe

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      !  Check flags from geogrid and the various model settings for any inconsistency.
      !  The optional data from geogrid mostly started with the v4.0 release. Older 
      !  WPS data could have the required fields. Assume users know what they are 
      !  doing when they bring old data into the newer real program ...

      IF ( grid%v4_metgrid ) THEN

         geogrid_flag_error = 0

         IF ( ( config_flags%topo_wind          .EQ. 1          ) .AND. ( flag_var_sso    .EQ. 0 ) ) THEN
            CALL wrf_message ( '----- ERROR: topo_wind          = 1 AND flag_var_sso    = 0 ' )
            geogrid_flag_error = geogrid_flag_error + 1
         END IF

         IF ( ( config_flags%sf_lake_physics    .EQ. 1           ) .AND. ( flag_lake_depth .EQ. 0 ) ) THEN
            CALL wrf_message ( '----- ERROR: sf_lake_physics    = 1 AND flag_lake_depth = 0 ' )
            geogrid_flag_error = geogrid_flag_error + 1
         END IF

         IF ( ( config_flags%sf_surface_physics .EQ. pxlsmscheme  ) .AND. ( flag_imperv    .EQ. 0 ) ) THEN
            CALL wrf_message ( '----- ERROR: sf_surface_physics = 7 AND flag_imperv     = 0 ' )
            geogrid_flag_error = geogrid_flag_error + 1
         END IF

         IF ( ( config_flags%sf_surface_physics .EQ. pxlsmscheme  ) .AND. ( flag_canfra    .EQ. 0 ) ) THEN
            CALL wrf_message ( '----- ERROR: sf_surface_physics = 7 AND flag_canfra     = 0 ' )
            geogrid_flag_error = geogrid_flag_error + 1
         END IF

         IF ( ( config_flags%mp_physics         .EQ. thompsonaero ) .AND. &
              ( config_flags%dust_emis          .EQ. 1            ) .AND. ( flag_erod      .EQ. 0 ) ) THEN
            CALL wrf_message ( '----- ERROR: mp=28 AND dust_emis= 1 AND flag_erod       = 0 ' )
            geogrid_flag_error = geogrid_flag_error + 1
         END IF

         IF ( ( config_flags%mp_physics         .EQ. thompsonaero ) .AND. &
              ( config_flags%dust_emis          .EQ. 1            ) .AND. ( flag_clayfrac  .EQ. 0 ) ) THEN
            CALL wrf_message ( '----- ERROR: mp=28 AND dust_emis= 1 AND flag_clayfrac   = 0 ' )
            geogrid_flag_error = geogrid_flag_error + 1
         END IF

         IF ( ( config_flags%mp_physics         .EQ. thompsonaero ) .AND. &
              ( config_flags%dust_emis          .EQ. 1            ) .AND. ( flag_sandfrac  .EQ. 0 ) ) THEN
            CALL wrf_message ( '----- ERROR: mp=28 AND dust_emis= 1 AND flag_sandfrac   = 0 ' )
            geogrid_flag_error = geogrid_flag_error + 1
         END IF
        
         IF ( geogrid_flag_error .GT. 0 ) THEN
            CALL wrf_error_fatal ('Either modify the namelist settings, or rebuild the geogrid/metgrid data' )
         END IF

         !  Geogrid flags that are not yet used: FLAG_FRC_URB2D FLAG_LAI12M FLAG_URB_PARAM
   
      END IF

      !  Would the user prefer to forego the use of the level of max winds, or the
      !  tropopause level data?  This is an option the user may select.  While the
      !  additional data is able to provide good information (such as a better
      !  resolution of the jet, a better kink for the tropopause), there are
      !  horizontal gradients that are introduced.  Near a boundary, these gradients
      !  would be permanent (due to their inclusion in the LBC file).  To turn "off"
      !  the use of the max wind/trop data, set the flags for those levels to zero.

      flag_pmaxw   = 0
      flag_pmaxwnn = 0
      flag_ptrop   = 0
      flag_ptropnn = 0
      IF ( ( config_flags%use_maxw_level .EQ. 0 ) .AND. &
           ( ( flag_tmaxw .EQ. 1 ) .OR. ( flag_umaxw .EQ. 1 ) .OR. ( flag_vmaxw .EQ. 1 ) .OR. ( flag_hgtmaxw .EQ. 1 ) ) ) THEN
         flag_tmaxw   = 0
         flag_umaxw   = 0
         flag_vmaxw   = 0
         flag_hgtmaxw = 0
         CALL wrf_debug ( 0 , 'Turning off use of MAX WIND level data in vertical interpolation' )
      END IF
      IF ( ( config_flags%use_trop_level .EQ. 0 ) .AND. &
           ( ( flag_ttrop .EQ. 1 ) .OR. ( flag_utrop .EQ. 1 ) .OR. ( flag_vtrop .EQ. 1 ) .OR. ( flag_hgttrop .EQ. 1 ) ) ) THEN
         flag_ttrop   = 0
         flag_utrop   = 0
         flag_vtrop   = 0
         flag_hgttrop = 0
         CALL wrf_debug ( 0 , 'Turning off use of TROPOPAUSE level data in vertical interpolation' )
      END IF

      !   Lake Mask and depth assignment

      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL nl_get_islake  ( grid%id , grid%islake )

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( grid%lu_index(i,j) .NE. grid%islake ) THEN
               grid%lakemask(i,j) = 0
            ELSE
               grid%lakemask(i,j) = 1
            END IF
         END DO
      END DO

      IF ( grid%sf_lake_physics .EQ. 1 ) THEN
         grid%lake_depth_flag = flag_lake_depth
         IF ( flag_lake_depth .EQ. 0 ) THEN
            CALL wrf_message ( " Warning: Please rerun WPS to get lake_depth information for lake model" )

         !  Set lake depth over the ocean to be -3 m, and set the lake depth over land to be -2 m.

         ELSE
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF      ( ( grid%lu_index(i,j) .NE. grid%islake ) .AND. ( grid%lu_index(i,j) .NE. grid%iswater ) ) THEN
                     grid%lake_depth(i,j) = -2
                  ELSE IF   ( grid%lu_index(i,j) .NE. grid%islake ) THEN
                     grid%lake_depth(i,j) = -3
                  END IF
               END DO
            END DO
         END IF
      END IF

      !  Send out a quick message about the time steps based on the map scale factors.

      IF ( ( internal_time_loop .EQ. 1 ) .AND. ( grid%id .EQ. 1 ) .AND. &
           ( .NOT. config_flags%polar ) ) THEN
         max_mf = grid%msft(its,jts)
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               max_mf = MAX ( max_mf , grid%msft(i,j) )
            END DO
         END DO
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
         max_mf = wrf_dm_max_real ( max_mf )
#endif
         WRITE ( a_message , FMT='(A,F5.2,A)' ) 'Max map factor in domain 1 = ',max_mf, &
                                                '. Scale the dt in the model accordingly.'
         CALL wrf_message ( a_message )
      END IF

      !  Check to see if the boundary conditions are set properly in the namelist file.
      !  This checks for sufficiency and redundancy.

      CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

      !  Some sort of "this is the first time" initialization.  Who knows.

      grid%step_number = 0
      grid%itimestep=0

      !  Pull in the info in the namelist to compare it to the input data.

      grid%real_data_init_type = model_config_rec%real_data_init_type

      !  To define the base state, we call a USER MODIFIED routine to set the three
      !  necessary constants:  p00 (sea level pressure, Pa), t00 (sea level temperature, K),
      !  and A (temperature difference, from 1000 mb to 300 mb, K).
   
      CALL const_module_initialize ( p00 , t00 , a , tiso , p_strat , a_strat )

      !  Save these constants to write out in model output file

      grid%t00  = t00
      grid%p00  = p00
      grid%tlp  = a
      grid%tiso = tiso
      grid%p_strat = p_strat
      grid%tlp_strat = a_strat

      !  Are there any hold-ups to us bypassing the middle of the domain?  These
      !  holdups would be situations where we need data in the middle of the domain.
      !  FOr example, if this si the first time period, we need the full domain
      !  processed for ICs.  Also, if there is some sort of gridded FDDA turned on, or
      !  if the SST update is activated, then we can't just blow off the middle of the 
      !  domain all willy-nilly.  Other cases of these hold-ups?  Sure - what if the
      !  user wants to smooth the CG topo, we need several rows and columns available.
      !  What if the lat/lon proj is used, then we need to run a spectral filter on
      !  the topo.  Both are killers when trying to ignore data in the middle of the
      !  domain.

      !  If hold_ups = .F., then there are no hold-ups to excluding the middle
      !  domain processing.  If hold_ups = .T., then there are hold-ups, and we
      !  must process the middle of the domain.

      hold_ups = ( internal_time_loop .EQ. 1 ) .OR. &
                 ( config_flags%grid_fdda .NE. 0 ) .OR. &
                 ( config_flags%sst_update .EQ. 1 ) .OR. &
                 ( config_flags%all_ic_times ) .OR. &
                 ( config_flags%polar )

      !  There are a few checks that we need to do when the input data comes in with the middle
      !  excluded by WPS.

      IF      ( flag_excluded_middle .NE. 0 ) THEN

         !  If this time period of data from WPS has the middle excluded, it had better be OK for
         !  us to have a hole.

         IF ( hold_ups ) THEN
            WRITE ( a_message,* ) 'None of the following are allowed to be TRUE : '
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) ' ( internal_time_loop .EQ. 1 )               ', ( internal_time_loop .EQ. 1 )
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) ' ( config_flags%grid_fdda .NE. 0 )           ', ( config_flags%grid_fdda .NE. 0 )
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) ' ( config_flags%sst_update .EQ. 1 )          ', ( config_flags%sst_update .EQ. 1 )
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) ' ( config_flags%all_ic_times )               ', ( config_flags%all_ic_times )
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) ' ( config_flags%smooth_cg_topo )             ', ( config_flags%smooth_cg_topo )
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) ' ( config_flags%polar )                      ', ( config_flags%polar )
            CALL wrf_message ( a_message )

            WRITE ( a_message,* ) 'Problems, we cannot have excluded middle data from WPS'
            CALL wrf_error_fatal ( a_message )
         END IF

         !  Make sure that the excluded middle data from metgrid is "wide enough".  We only have to check
         !  when the excluded middle was actually used in WPS.

         IF ( config_flags%spec_bdy_width .GT. flag_excluded_middle ) THEN
            WRITE ( a_message,* ) 'The WRF &bdy_control namelist.input spec_bdy_width = ', config_flags%spec_bdy_width
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) 'The WPS &metgrid namelist.wps process_only_bdy width = ',flag_excluded_middle
            CALL wrf_message ( a_message )
            WRITE ( a_message,* ) 'WPS process_only_bdy must be >= WRF spec_bdy_width'
            CALL wrf_error_fatal ( a_message )
         END IF
      END IF
      em_width = config_flags%spec_bdy_width

      !  We need to find if there are any valid non-excluded-middle points in this
      !  tile.  If so, then we need to hang on to a valid i,j location.

      any_valid_points = .false.
      find_valid : DO j = jts,jte
         DO i = its,ite
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            any_valid_points = .true.
            i_valid = i
            j_valid = j
            EXIT find_valid
         END DO
      END DO find_valid

      !  Replace traditional seaice field with optional seaice (AFWA source)

      IF ( flag_icefrac .EQ. 1 ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%xice(i,j) = grid%icefrac_gc(i,j)
            END DO
         END DO
      END IF

      !  Replace traditional seaice field with optional seaice percent (AFWA source)

      IF ( flag_icepct .EQ. 1 ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%xice(i,j) = grid%icepct(i,j)/100.
            END DO
         END DO
      END IF

      !  Fix the snow (water equivalent depth, kg/m^2) and the snowh (physical snow
      !  depth, m) fields.

      IF      ( ( flag_snow .EQ. 0 ) .AND. ( flag_snowh .EQ. 0 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%snow(i,j)  = 0.
               grid%snowh(i,j) = 0.
            END DO
         END DO

      ELSE IF ( ( flag_snow .EQ. 0 ) .AND. ( flag_snowh .EQ. 1 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
!              ( m -> kg/m^2 )  & ( reduce to liquid, 5:1 ratio )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%snow(i,j)  = grid%snowh(i,j) * 1000. / 5.
            END DO
         END DO

      ELSE IF ( ( flag_snow .EQ. 1 ) .AND. ( flag_snowh .EQ. 0 ) ) THEN
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
!              ( kg/m^2 -> m)  & ( liquid to snow depth, 5:1 ratio )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%snowh(i,j) = grid%snow(i,j) / 1000. * 5.
            END DO
         END DO

      END IF

      !  For backward compatibility, we might need to assign the map factors from
      !  what they were, to what they are.

      IF      ( ( config_flags%polar ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         DO j=max(jds+1,jts),min(jde-1,jte)
            DO i=its,min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
         IF(jts == jds) THEN
            DO i=its,ite
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%msfvx(i,jts) = 0.
               grid%msfvx_inv(i,jts) = 0.
            END DO
         END IF
         IF(jte == jde) THEN
            DO i=its,ite
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%msfvx(i,jte) = 0.
               grid%msfvx_inv(i,jte) = 0.
            END DO
         END IF
      ELSE IF ( ( .NOT. config_flags%polar ) .AND. ( flag_mf_xy .EQ. 1 ) ) THEN
         IF ( grid%msfvx(its,jts) .EQ. 0 ) THEN
            CALL wrf_error_fatal ( 'Maybe this is a global domain, but the polar flag was not set in the bdy_control namelist.' )
         END IF
         DO j=jts,min(jde,jte)
            DO i=its,min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%msfvx_inv(i,j) = 1./grid%msfvx(i,j)
            END DO
         END DO
      ELSE IF ( ( config_flags%polar ) .AND. ( flag_mf_xy .NE. 1 ) ) THEN
         CALL wrf_error_fatal ( 'Older metgrid data cannot initialize a global domain' )
      ENDIF

      !  Check to see what available surface temperatures we have.

      IF ( flag_tavgsfc .EQ. 1 ) THEN
         we_have_tavgsfc = .TRUE.
      ELSE
         we_have_tavgsfc = .FALSE.
      END IF

      IF ( flag_tsk     .EQ. 1 ) THEN
         we_have_tsk     = .TRUE.
      ELSE
         we_have_tsk     = .FALSE.
      END IF
   
      IF ( config_flags%use_tavg_for_tsk ) THEN
         IF ( we_have_tsk .OR. we_have_tavgsfc ) THEN
           !  we are OK
         ELSE
            CALL wrf_error_fatal ( 'We either need TSK or TAVGSFC, verify these fields are coming from WPS' )
         END IF
   
         !  Since we require a skin temperature in the model, we can use the average 2-m temperature if provided.
   
         IF ( we_have_tavgsfc ) THEN
            DO j=jts,min(jde,jte)
               DO i=its,min(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%tsk(i,j) = grid%tavgsfc(i,j)
               END DO
            END DO
         END IF
      END IF

      !  Is there any vertical interpolation to do?  The "old" data comes in on the correct
      !  vertical locations already.

      IF ( flag_metgrid .EQ. 1 ) THEN  !   <----- START OF VERTICAL INTERPOLATION PART ---->

         num_metgrid_levels = grid%num_metgrid_levels

         IF ( config_flags%nest_interp_coord .EQ. 1 ) THEN

            !  At the location of maximum pressure in the column, get the temperature and height.  These
            !  will be written out and could be used for vertical interpolation - to avoid extrapolation.
            !  Hey, we can also do minimum values, too.
   
            DO j=jts,jte
               DO i=its,ite
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%max_p(i,j) = grid%p_gc(i,1,j)
                  k_max_p = 1
                  IF      ( grid%p_gc(i,2,j) .GT. grid%max_p(i,j) ) THEN
                     grid%max_p(i,j) = grid%p_gc(i,2,j)
                     k_max_p = 2
                  ELSE IF ( grid%p_gc(i,num_metgrid_levels,j) .GT. grid%max_p(i,j) ) THEN
                     grid%max_p(i,j) = grid%p_gc(i,num_metgrid_levels,j)
                     k_max_p = num_metgrid_levels
                  END IF
                  grid%t_max_p(i,j) = grid%t_gc(i,k_max_p,j)
                  grid%ght_max_p(i,j) = grid%ght_gc(i,k_max_p,j)
   
                  grid%min_p(i,j) = grid%p_gc(i,num_metgrid_levels,j)
                  k_min_p = num_metgrid_levels
                  IF      ( grid%p_gc(i,2,j) .LT. grid%min_p(i,j) ) THEN
                     grid%min_p(i,j) = grid%p_gc(i,2,j)
                     k_min_p = 2
                  END IF
                  grid%t_min_p(i,j) = grid%t_gc(i,k_min_p,j)
                  grid%ght_min_p(i,j) = grid%ght_gc(i,k_min_p,j)
               END DO
            END DO
         END IF

         !  If this is data from the PINTERP program, it is emulating METGRID output.
         !  One of the caveats of this data is the way that the vertical structure is
         !  handled.  We take the k=1 level and toss it (it is disposable), and we
         !  swap in the surface data.  This is done for all of the 3d fields about
         !  which we show some interest: u, v, t, rh, ght, and p.  For u, v, and rh,
         !  we assume no interesting vertical structure, and just assign the 1000 mb
         !  data.  We directly use the 2-m temp for surface temp.  We use the surface
         !  pressure field and the topography elevation for the lowest level of
         !  pressure and height, respectively.

         IF ( flag_pinterp .EQ. 1 ) THEN

            WRITE ( a_message , * ) 'Data from P_INTERP program, filling k=1 level with artificial surface fields.'
            CALL wrf_message ( a_message )
            DO j=jts,jte
               DO i=its,ite
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%u_gc(i,1,j) = grid%u_gc(i,2,j)
                  grid%v_gc(i,1,j) = grid%v_gc(i,2,j)
                  grid%rh_gc(i,1,j) = grid%rh_gc(i,2,j)
                  grid%t_gc(i,1,j) = grid%t2(i,j)
                  grid%ght_gc(i,1,j) = grid%ht(i,j)
                  grid%p_gc(i,1,j) = grid%psfc(i,j)
               END DO
            END DO
            flag_psfc = 0

         END IF

         !  Variables that are named differently between SI and WPS.

         DO j = jts, MIN(jte,jde-1)
            DO i = its, MIN(ite,ide-1)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%tsk(i,j) = grid%tsk_gc(i,j)
               grid%tmn(i,j) = grid%tmn_gc(i,j)
               grid%xlat(i,j) = grid%xlat_gc(i,j)
               grid%xlong(i,j) = grid%xlong_gc(i,j)
               grid%ht(i,j) = grid%ht_gc(i,j)
            END DO
         END DO

         !  A user could request that the most coarse grid has the
         !  topography along the outer boundary smoothed.  This smoothing
         !  is similar to the coarse/nest interface.  The outer rows and
         !  cols come from the existing large scale topo, and then the
         !  next several rows/cols are a linear ramp of the large scale
         !  model and the hi-res topo from WPS.  We only do this for the
         !  coarse grid since we are going to make the interface consistent
         !  in the model betwixt the CG and FG domains.

         IF ( ( config_flags%smooth_cg_topo ) .AND. &
              ( internal_time_loop .EQ. 1 ) .AND. &
              ( grid%id .EQ. 1 ) .AND. &
              ( flag_soilhgt .EQ. 1) ) THEN
            CALL blend_terrain ( grid%toposoil  , grid%ht , &
                                 ids , ide , jds , jde , 1   , 1   , &
                                 ims , ime , jms , jme , 1   , 1   , &
                                 ips , ipe , jps , jpe , 1   , 1   )
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%ht_smooth(i,j) = grid%ht(i,j)
               END DO
            END DO

         ELSE IF ( ( config_flags%smooth_cg_topo ) .AND. &
              ( internal_time_loop .NE. 1 ) .AND. &
              ( grid%id .EQ. 1 ) .AND. &
              ( flag_soilhgt .EQ. 1) ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%ht(i,j) = grid%ht_smooth(i,j)
               END DO
            END DO
            
         END IF

         !  Filter the input topography if this is a global domain.

         IF ( ( config_flags%polar ) .AND. ( grid%fft_filter_lat .GT. 90 ) ) THEN
            CALL wrf_error_fatal ( 'If the polar boundary condition is used, then fft_filter_lat must be set in namelist.input' )
         END IF

         IF ( ( config_flags%map_proj .EQ. PROJ_CASSINI ) .AND. ( config_flags%polar ) ) THEN
#if 1
            dclat = 90./REAL(jde-jds) !(0.5 * 180/ny)
            DO j = jts, MIN(jte,jde-1)
              DO k = kts, kte
                 DO i = its, MIN(ite,ide-1)
                    grid%t_2(i,k,j) = 1.
                 END DO
              END DO
              DO i = its, MIN(ite,ide-1)
                 grid%t_2(i,1,j) = grid%ht(i,j)
                 grid%sr(i,j) = grid%ht(i,j)
              END DO
            END DO
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
! WARNING:  this might present scaling issues on very large numbers of processors
     ALLOCATE( clat_glob(ids:ide,jds:jde) )

     CALL wrf_patch_to_global_real ( grid%clat, clat_glob, grid%domdesc, 'xy', 'xy', &
                                     ids, ide, jds, jde, 1, 1, &
                                     ims, ime, jms, jme, 1, 1, &
                                     its, ite, jts, jte, 1, 1 )

     CALL wrf_dm_bcast_real ( clat_glob , (ide-ids+1)*(jde-jds+1) )

     grid%clat_xxx(ipsx:ipex,jpsx:jpex) = clat_glob(ipsx:ipex,jpsx:jpex)

     find_j_index_of_fft_filter : DO j = jds , jde-1
        IF ( ABS(clat_glob(ids,j)) .LE. config_flags%fft_filter_lat ) THEN
           j_save = j
           EXIT find_j_index_of_fft_filter
        END IF
     END DO find_j_index_of_fft_filter

     CALL wrf_patch_to_global_real ( grid%msft, clat_glob, grid%domdesc, 'xy', 'xy', &
                                     ids, ide, jds, jde, 1, 1, &
                                     ims, ime, jms, jme, 1, 1, &
                                     its, ite, jts, jte, 1, 1 )

     CALL wrf_dm_bcast_real ( clat_glob , (ide-ids+1)*(jde-jds+1) )

     grid%mf_fft = clat_glob(ids,j_save)

     grid%mf_xxx(ipsx:ipex,jpsx:jpex) = clat_glob(ipsx:ipex,jpsx:jpex)

     DEALLOCATE( clat_glob )
#else
     find_j_index_of_fft_filter : DO j = jds , jde-1
        IF ( ABS(grid%clat(ids,j)) .LE. config_flags%fft_filter_lat ) THEN
           j_save = j
           EXIT find_j_index_of_fft_filter
        END IF
     END DO find_j_index_of_fft_filter
     grid%mf_fft = grid%msft(ids,j_save)
#endif

         CALL pxft ( grid=grid                                              &
               ,lineno=__LINE__                                             &
               ,flag_uv            = 0                                      &
               ,flag_rurv          = 0                                      &
               ,flag_wph           = 0                                      &
               ,flag_ww            = 0                                      &
               ,flag_t             = 1                                      &
               ,flag_mu            = 0                                      &
               ,flag_mut           = 0                                      &
               ,flag_moist         = 0                                      &
               ,flag_chem          = 0                                      &
               ,flag_tracer        = 0                                      &
               ,flag_scalar        = 0                                      &
               ,actual_distance_average  = .TRUE.                           &
               ,pos_def            = .FALSE.                                &
               ,swap_pole_with_next_j = .FALSE.                             &
               ,moist=moist,chem=chem,tracer=tracer,scalar=scalar           &
               ,fft_filter_lat = config_flags%fft_filter_lat                &
               ,dclat = dclat                                               &
               ,ids=ids,ide=ide,jds=jds,jde=jde,kds=kds,kde=kde             &
               ,ims=ims,ime=ime,jms=jms,jme=jme,kms=kms,kme=kme             &
               ,ips=ips,ipe=ipe,jps=jps,jpe=jpe,kps=kps,kpe=kpe             &
               ,imsx=imsx,imex=imex,jmsx=jmsx,jmex=jmex,kmsx=kmsx,kmex=kmex &
               ,ipsx=ipsx,ipex=ipex,jpsx=jmsx,jpex=jpex,kpsx=kpsx,kpex=kpex )

            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 grid%ht(i,j) = grid%t_2(i,1,j)
                 grid%sr(i,j) = grid%sr(i,j) - grid%ht(i,j)
              END DO
            END DO

#else
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )

            !  We stick the topo and map fac in an unused 3d array. The map scale
            !  factor and computational latitude are passed along for the ride
            !  (part of the transpose process - we only do 3d arrays) to determine
            !  "how many" values are used to compute the mean.  We want a number
            !  that is consistent with the original grid resolution.

            DO j = jts, MIN(jte,jde-1)
              DO k = kts, kte
                 DO i = its, MIN(ite,ide-1)
                    grid%t_init(i,k,j) = 1.
                 END DO
              END DO
              DO i = its, MIN(ite,ide-1)
                 grid%t_init(i,1,j) = grid%ht(i,j)
                 grid%t_init(i,2,j) = grid%msftx(i,j)
                 grid%t_init(i,3,j) = grid%clat(i,j)
              END DO
            END DO

# include "XPOSE_POLAR_FILTER_TOPO_z2x.inc"

            !  Retrieve the 2d arrays for topo, map factors, and the
            !  computational latitude.

            DO j = jpsx, MIN(jpex,jde-1)
              DO i = ipsx, MIN(ipex,ide-1)
                 grid%ht_xxx(i,j)   = grid%t_xxx(i,1,j)
                 grid%mf_xxx(i,j)   = grid%t_xxx(i,2,j)
                 grid%clat_xxx(i,j) = grid%t_xxx(i,3,j)
              END DO
            END DO

            !  Get a mean topo field that is consistent with the grid
            !  distance on each computational latitude loop.

            CALL filter_topo ( grid%ht_xxx , grid%clat_xxx , grid%mf_xxx , &
                               grid%fft_filter_lat , grid%mf_fft , &
                               .FALSE. , .FALSE. , &
                               ids, ide, jds, jde, 1 , 1 , &
                               imsx, imex, jmsx, jmex, 1, 1, &
                               ipsx, ipex, jpsx, jpex, 1, 1 )

            !  Stick the filtered topo back into the dummy 3d array to
            !  transpose it back to "all z on a patch".

            DO j = jpsx, MIN(jpex,jde-1)
              DO i = ipsx, MIN(ipex,ide-1)
                 grid%t_xxx(i,1,j) = grid%ht_xxx(i,j)
              END DO
            END DO

# include "XPOSE_POLAR_FILTER_TOPO_x2z.inc"

            !  Get the un-transposed topo data.

            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 grid%ht(i,j) = grid%t_init(i,1,j)
              END DO
            END DO
#else
            CALL filter_topo ( grid%ht , grid%clat , grid%msftx , &
                               grid%fft_filter_lat , grid%mf_fft , &
                               .FALSE. , .FALSE. , &
                               ids, ide, jds, jde, 1,1,           &
                               ims, ime, jms, jme, 1,1,           &
                               its, ite, jts, jte, 1,1 )
#endif
#endif
         ELSE IF ( ( config_flags%map_proj .NE. PROJ_CASSINI ) .AND. ( config_flags%polar ) ) THEN
            WRITE ( a_message,* ) 'A global domain (polar = true) requires the Cassini projection'
            CALL wrf_error_fatal ( a_message )
         END IF

         !  If we have any input low-res surface pressure, we store it.

         IF ( flag_psfc .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                 grid%psfc_gc(i,j) = grid%psfc(i,j)
                 grid%p_gc(i,1,j) = grid%psfc(i,j)
              END DO
            END DO
         END IF

         !  If we have the low-resolution surface elevation, stick that in the
         !  "input" locations of the 3d height.  We still have the "hi-res" topo
         !  stuck in the grid%ht array.  The grid%landmask if test is required as some sources
         !  have ZERO elevation over water (thank you very much).

         IF ( flag_soilhgt .EQ. 1) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
!                 IF ( grid%landmask(i,j) .GT. 0.5 ) THEN
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid%ght_gc(i,1,j) = grid%toposoil(i,j)
                     grid%ht_gc(i,j)= grid%toposoil(i,j)
!                 END IF
               END DO
           END DO
         END IF

         !  The number of vertical levels in the input data.  There is no staggering for
         !  different variables.

         num_metgrid_levels = grid%num_metgrid_levels

         !  For AFWA UM data, swap incoming extra (theta-based) pressure with the standardly
         !  named (rho-based) pressure.

         IF ( flag_ptheta .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     ptemp = grid%p_gc(i,k,j)
                     grid%p_gc(i,k,j) = grid%prho_gc(i,k,j)
                     grid%prho_gc(i,k,j) = ptemp
                  END DO
               END DO
            END DO
         END IF

         !  For UM data, the "surface" and the "first hybrid" level for the theta-level data fields are the same.  
         !  Average the surface (k=1) and the second hybrid level (k=num_metgrid_levels-1) to get the first hybrid
         !  layer.  We only do this for the theta-level data: pressure, temperature, specific humidity, and
         !  geopotential height (i.e. we do not modify u, v, or the rho-based pressure).

         IF ( ( flag_ptheta .EQ. 1 ) .OR. ( flag_prho .EQ. 1 ) ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%  p_gc(i,num_metgrid_levels,j) = ( grid%  p_gc(i,1,j) + grid%  p_gc(i,num_metgrid_levels-1,j) ) * 0.5
                  grid%  t_gc(i,num_metgrid_levels,j) = ( grid%  t_gc(i,1,j) + grid%  t_gc(i,num_metgrid_levels-1,j) ) * 0.5
                  grid%ght_gc(i,num_metgrid_levels,j) = ( grid%ght_gc(i,1,j) + grid%ght_gc(i,num_metgrid_levels-1,j) ) * 0.5
               END DO
            END DO

            IF ( grid%sh_gc(its,1,jts) .LT. 0 ) THEN
               DO j = jts, MIN(jte,jde-1)
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid% sh_gc(i,1,j) = 2. * grid% sh_gc(i,num_metgrid_levels,j) - grid% sh_gc(i,num_metgrid_levels-1,j)
                  END DO
               END DO
            END IF
            IF ( grid%cl_gc(its,1,jts) .LT. 0 ) THEN
               DO j = jts, MIN(jte,jde-1)
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid% cl_gc(i,1,j) = 2. * grid% cl_gc(i,num_metgrid_levels,j) - grid% cl_gc(i,num_metgrid_levels-1,j)
                  END DO
               END DO
            END IF
            IF ( grid%cf_gc(its,1,jts) .LT. 0 ) THEN
               DO j = jts, MIN(jte,jde-1)
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid% cf_gc(i,1,j) = 2. * grid% cf_gc(i,num_metgrid_levels,j) - grid% cf_gc(i,num_metgrid_levels-1,j)
                  END DO
               END DO
            END IF
         END IF

         !  For UM data, the soil moisture comes in as kg / m^2. Divide by 1000 and layer thickness to get m^3 / m^3.

         IF ( flag_prho .EQ. 1 ) THEN

            levels(1) = 0.
            levels(2) = ( 2. * sm_levels_input(1) )
            DO k = 2 , num_sm_levels_input
               levels(k+1) = ( 2. * sm_levels_input(k) ) - levels(k)
            END DO
            DO k = 1 , num_sm_levels_input
               thickness(k) = ( levels(k+1) - levels(k) ) / 100.
            END DO

            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_sm_levels_input
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     sm_input(i,k+1,j) = MAX ( 0. , sm_input(i,k+1,j) / 1000. / thickness(k) )
                  END DO
               END DO
            END DO
         END IF

         IF ( any_valid_points ) THEN
         !  Check for and semi-fix missing surface fields.

         IF ( grid%p_gc(i_valid,num_metgrid_levels,j_valid) .LT. grid%p_gc(i_valid,2,j_valid) ) THEN
            k = 2
         ELSE
            k = num_metgrid_levels
         END IF

         IF ( grid%t_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%t_gc(i,1,j) = grid%t_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface temp, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message )
         END IF

         IF ( grid%rh_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%rh_gc(i,1,j) = grid%rh_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface RH, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message )
         END IF

         IF ( grid%u_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO i = its, ite
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%u_gc(i,1,j) = grid%u_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface u wind, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message )
         END IF

         IF ( grid%v_gc(i_valid,1,j_valid) .EQ. -1.E30 ) THEN
            DO j = jts, jte
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%v_gc(i,1,j) = grid%v_gc(i,k,j)
               END DO
            END DO
            config_flags%use_surface = .FALSE.
            grid%use_surface = .FALSE.
            WRITE ( a_message , * ) 'Missing surface v wind, replaced with closest level, use_surface set to false.'
            CALL wrf_message ( a_message )
         END IF

         !  Compute the mixing ratio from the input relative humidity.

         IF ( ( flag_qv .NE. 1 ) .AND. ( flag_sh .NE. 1 ) ) THEN
            IF ( grid%p_gc(i_valid,num_metgrid_levels,j_valid) .LT. grid%p_gc(i_valid,2,j_valid) ) THEN
               k = 2
            ELSE
               k = num_metgrid_levels
            END IF

            IF      ( config_flags%rh2qv_method .eq. 1 ) THEN
               CALL rh_to_mxrat1(grid%rh_gc, grid%t_gc, grid%p_gc, grid%qv_gc ,         &
                                 config_flags%rh2qv_wrt_liquid ,                        &
                                 config_flags%qv_max_p_safe ,                           &
                                 config_flags%qv_max_flag , config_flags%qv_max_value , &
                                 config_flags%qv_min_p_safe ,                           &
                                 config_flags%qv_min_flag , config_flags%qv_min_value , &
                                 ids , ide , jds , jde , 1   , num_metgrid_levels ,     &
                                 ims , ime , jms , jme , 1   , num_metgrid_levels ,     &
                                 its , ite , jts , jte , 1   , num_metgrid_levels )
            ELSE IF ( config_flags%rh2qv_method .eq. 2 ) THEN
               CALL rh_to_mxrat2(grid%rh_gc, grid%t_gc, grid%p_gc, grid%qv_gc ,         &
                                 config_flags%rh2qv_wrt_liquid ,                        &
                                 config_flags%qv_max_p_safe ,                           &
                                 config_flags%qv_max_flag , config_flags%qv_max_value , &
                                 config_flags%qv_min_p_safe ,                           &
                                 config_flags%qv_min_flag , config_flags%qv_min_value , &
                                 ids , ide , jds , jde , 1   , num_metgrid_levels ,     &
                                 ims , ime , jms , jme , 1   , num_metgrid_levels ,     &
                                 its , ite , jts , jte , 1   , num_metgrid_levels )
            END IF


         ELSE IF ( flag_sh .EQ. 1 ) THEN
            IF ( grid%p_gc(i_valid,num_metgrid_levels,j_valid) .LT. grid%p_gc(i_valid,2,j_valid) ) THEN
               k = 2
            ELSE
               k = num_metgrid_levels
            END IF
            IF ( grid%sh_gc(i_valid,kts,j_valid) .LT. 1.e-6 ) THEN
               DO j = jts, MIN(jte,jde-1)
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid%sh_gc(i,1,j) = grid%sh_gc(i,k,j)
                  END DO
               END DO
            END IF

            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid%qv_gc(i,k,j) = grid%sh_gc(i,k,j) /( 1. - grid%sh_gc(i,k,j) )
                     sat_vap_pres_mb = 0.6112*10.*EXP(17.67*(grid%t_gc(i,k,j)-273.15)/(grid%t_gc(i,k,j)-29.65))
                     vap_pres_mb = grid%qv_gc(i,k,j) * grid%p_gc(i,k,j)/100. / (grid%qv_gc(i,k,j) + 0.622 )
                     IF ( sat_vap_pres_mb .GT. 0 ) THEN
                        grid%rh_gc(i,k,j) = ( vap_pres_mb / sat_vap_pres_mb ) * 100.
                     ELSE
                        grid%rh_gc(i,k,j) = 0.
                     END IF
                  END DO
               END DO
            END DO

         ELSE IF ( flag_qv .EQ. 1 ) THEN
            IF ( grid%p_gc(i_valid,num_metgrid_levels,j_valid) .LT. grid%p_gc(i_valid,2,j_valid) ) THEN
               k = 2
            ELSE
               k = num_metgrid_levels
            END IF

            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     sat_vap_pres_mb = 0.6112*10.*EXP(17.67*(grid%t_gc(i,k,j)-273.15)/(grid%t_gc(i,k,j)-29.65))
                     vap_pres_mb = grid%qv_gc(i,k,j) * grid%p_gc(i,k,j)/100. / (grid%qv_gc(i,k,j) + 0.622 )
                     IF ( sat_vap_pres_mb .GT. 0 ) THEN
                        grid%rh_gc(i,k,j) = ( vap_pres_mb / sat_vap_pres_mb ) * 100.
                     ELSE
                        grid%rh_gc(i,k,j) = 0.
                     END IF
                  END DO
               END DO
            END DO

         END IF

         !  Some data sets do not provide a 3d geopotential height field.  

         IF ( grid%ght_gc(i_valid,grid%num_metgrid_levels/2,j_valid) .LT. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = kts+1 , grid%num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid%ght_gc(i,k,j) = grid%ght_gc(i,k-1,j) - &
                        R_d / g * 0.5 * ( grid%t_gc(i,k  ,j) * ( 1 + 0.608 * grid%qv_gc(i,k  ,j) ) +   &
                                          grid%t_gc(i,k-1,j) * ( 1 + 0.608 * grid%qv_gc(i,k-1,j) ) ) * &
                        LOG ( grid%p_gc(i,k,j) / grid%p_gc(i,k-1,j) )
                  END DO
               END DO
            END DO
         END IF

         !  If the pressure levels in the middle of the atmosphere are upside down, then
         !  this is hybrid data.  Computing the new surface pressure should use sfcprs2.

         IF ( grid%p_gc(i_valid,num_metgrid_levels/2,j_valid) .LT. grid%p_gc(i_valid,num_metgrid_levels/2+1,j_valid) ) THEN
            config_flags%sfcp_to_sfcp = .TRUE.
         END IF
         END IF

         !  Assign surface fields with original input values.  If this is hybrid data,
         !  the values are not exactly representative.  However - this is only for
         !  plotting purposes and such at the 0h of the forecast, so we are not all that
         !  worried.

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%u10(i,j)=grid%u_gc(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%v10(i,j)=grid%v_gc(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%t2(i,j)=grid%t_gc(i,1,j)
            END DO
         END DO

         IF ( flag_qv .EQ. 1 ) THEN
            DO j = jts, min(jde-1,jte)
               DO i = its, min(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%q2(i,j)=grid%qv_gc(i,1,j)
               END DO
            END DO
         END IF

         !  The requested ptop for real data cases.

         p_top_requested = grid%p_top_requested

         !  Compute the top pressure, grid%p_top.  For isobaric data, this is just the
         !  top level.  For the generalized vertical coordinate data, we find the
         !  max pressure on the top level.  We have to be careful of two things:
         !  1) the value has to be communicated, 2) the value can not increase
         !  at subsequent times from the initial value.

         IF ( internal_time_loop .EQ. 1 ) THEN
            CALL find_p_top ( grid%p_gc , grid%p_top , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
            grid%p_top = wrf_dm_max_real ( grid%p_top )
#endif

            !  Compare the requested grid%p_top with the value available from the input data.

            IF ( p_top_requested .LT. grid%p_top ) THEN
               print *,'p_top_requested = ',p_top_requested
               print *,'allowable grid%p_top in data   = ',grid%p_top
               CALL wrf_error_fatal ( 'p_top_requested < grid%p_top possible from data' )
            END IF

            !  The grid%p_top valus is the max of what is available from the data and the
            !  requested value.  We have already compared <, so grid%p_top is directly set to
            !  the value in the namelist.

            grid%p_top = p_top_requested

            !  For subsequent times, we have to remember what the grid%p_top for the first
            !  time was.  Why?  If we have a generalized vert coordinate, the grid%p_top value
            !  could fluctuate.

            p_top_save = grid%p_top

         ELSE
            CALL find_p_top ( grid%p_gc , grid%p_top , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )

#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
            grid%p_top = wrf_dm_max_real ( grid%p_top )
#endif
            IF ( grid%p_top .GT. p_top_save ) THEN
               print *,'grid%p_top from last time period = ',p_top_save
               print *,'grid%p_top from this time period = ',grid%p_top
               CALL wrf_error_fatal ( 'grid%p_top > previous value' )
            END IF
            grid%p_top = p_top_save
         ENDIF

         !  Get the monthly values interpolated to the current date for the traditional monthly
         !  fields of green-ness fraction and background albedo.

         CALL monthly_interp_to_date ( grid%greenfrac , current_date , grid%vegfra , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         CALL monthly_interp_to_date ( grid%albedo12m , current_date , grid%albbck , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

         CALL monthly_interp_to_date ( grid%lai12m , current_date , grid%lai , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )

#if ( WRF_CHEM == 1 )
         ! Chose the appropriate LAI veg mask for this date (used in the AFWA dust model)

         CALL eightday_selector ( grid%lai_veg_8day , current_date , grid%lai_vegmask , &
                                       ids , ide , jds , jde , kds , kde , &
                                       ims , ime , jms , jme , kms , kme , &
                                       its , ite , jts , jte , kts , kte )
#endif

         !  Get the min/max of each i,j for the monthly green-ness fraction.

         CALL monthly_min_max ( grid%greenfrac , grid%shdmin , grid%shdmax , &
                                ids , ide , jds , jde , kds , kde , &
                                ims , ime , jms , jme , kms , kme , &
                                its , ite , jts , jte , kts , kte )

         !  The model expects the green-ness and vegetation fraction values to be in percent, not fraction.

         DO j = jts, MIN(jte,jde-1)
           DO i = its, MIN(ite,ide-1)
              IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
              grid%vegfra(i,j) = grid%vegfra(i,j) * 100.
              grid%shdmax(i,j) = grid%shdmax(i,j) * 100.
              grid%shdmin(i,j) = grid%shdmin(i,j) * 100.
           END DO
         END DO

         !  The model expects the albedo fields as a fraction, not a percent.  Set the
         !  water values to 8%.

         DO j = jts, MIN(jte,jde-1)
            DO i = its, MIN(ite,ide-1)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%albbck(i,j) = grid%albbck(i,j) / 100.
               grid%snoalb(i,j) = grid%snoalb(i,j) / 100.
               IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                  grid%albbck(i,j) = 0.08
                  grid%snoalb(i,j) = 0.08
               END IF
            END DO
         END DO

         !  Added by G. Thompson 2013Sep10
         !  Interpolate monthly aerosol climatology data to specific date/time.
         !  Since data are 3D, do over a loop of vertical levels using temporary array space.

         IF (config_flags%mp_physics.eq.THOMPSONAERO .and. P_QNWFA.gt.1 .and. config_flags%use_aero_icbc) then
         CALL wrf_debug ( 0 , 'Using Thompson_aerosol_aware so using QNWFA monthly climo arrays to create QNWFA_now')
         DO k = 1, num_metgrid_levels
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%qntemp(i, 1, j) = grid%QNWFA_jan(i,k,j)
                  grid%qntemp(i, 2, j) = grid%QNWFA_feb(i,k,j)
                  grid%qntemp(i, 3, j) = grid%QNWFA_mar(i,k,j)
                  grid%qntemp(i, 4, j) = grid%QNWFA_apr(i,k,j)
                  grid%qntemp(i, 5, j) = grid%QNWFA_may(i,k,j)
                  grid%qntemp(i, 6, j) = grid%QNWFA_jun(i,k,j)
                  grid%qntemp(i, 7, j) = grid%QNWFA_jul(i,k,j)
                  grid%qntemp(i, 8, j) = grid%QNWFA_aug(i,k,j)
                  grid%qntemp(i, 9, j) = grid%QNWFA_sep(i,k,j)
                  grid%qntemp(i,10, j) = grid%QNWFA_oct(i,k,j)
                  grid%qntemp(i,11, j) = grid%QNWFA_nov(i,k,j)
                  grid%qntemp(i,12, j) = grid%QNWFA_dec(i,k,j)
               ENDDO
            ENDDO

            CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                          ids , ide , jds , jde , kds , kde , &
                                          ims , ime , jms , jme , kms , kme , &
                                          its , ite , jts , jte , kts , kte )

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%QNWFA_now(i,k,j) = grid%qntemp2(i,j)
     if(i.eq.its .and. j.eq.jts .and. k.eq.1) then
      write(a_message,*) '  DEBUG, ensuring QNWFA_now corner (its,1,jts) value: ', i,j,grid%QNWFA_now(i,k,j)
      CALL wrf_debug (1, a_message)
     endif
               ENDDO
            ENDDO
         ENDDO
         ENDIF

         IF (config_flags%mp_physics.eq.THOMPSONAERO .and. P_QNIFA.gt.1 .and. config_flags%use_aero_icbc) then
         DO k = 1, num_metgrid_levels
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%qntemp(i, 1, j) = grid%QNIFA_jan(i,k,j)
                  grid%qntemp(i, 2, j) = grid%QNIFA_feb(i,k,j)
                  grid%qntemp(i, 3, j) = grid%QNIFA_mar(i,k,j)
                  grid%qntemp(i, 4, j) = grid%QNIFA_apr(i,k,j)
                  grid%qntemp(i, 5, j) = grid%QNIFA_may(i,k,j)
                  grid%qntemp(i, 6, j) = grid%QNIFA_jun(i,k,j)
                  grid%qntemp(i, 7, j) = grid%QNIFA_jul(i,k,j)
                  grid%qntemp(i, 8, j) = grid%QNIFA_aug(i,k,j)
                  grid%qntemp(i, 9, j) = grid%QNIFA_sep(i,k,j)
                  grid%qntemp(i,10, j) = grid%QNIFA_oct(i,k,j)
                  grid%qntemp(i,11, j) = grid%QNIFA_nov(i,k,j)
                  grid%qntemp(i,12, j) = grid%QNIFA_dec(i,k,j)
               ENDDO
            ENDDO
            CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                          ids , ide , jds , jde , kds , kde , &
                                          ims , ime , jms , jme , kms , kme , &
                                          its , ite , jts , jte , kts , kte )
            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%QNIFA_now(i,k,j) = grid%qntemp2(i,j)
               ENDDO
            ENDDO
         ENDDO
         ENDIF

         !  Two ways to get the surface pressure.  1) If we have the low-res input surface
         !  pressure and the low-res topography, then we can do a simple hydrostatic
         !  relation.  2) Otherwise we compute the surface pressure from the sea-level
         !  pressure.
         !  Note that on output, grid%psfc is now hi-res.  The low-res surface pressure and
         !  elevation are grid%psfc_gc and grid%ht_gc (same as grid%ght_gc(k=1)).

         IF      ( ( flag_psfc    .EQ. 1 ) .AND. &
                   ( flag_soilhgt .EQ. 1 ) .AND. &
                   ( flag_slp     .EQ. 1 ) .AND. &
                   ( .NOT. config_flags%sfcp_to_sfcp ) ) THEN
            WRITE(a_message,FMT='(A)') 'Using sfcprs3 to compute psfc'
            CALL wrf_message ( a_message )
            CALL sfcprs3(grid%ght_gc, grid%p_gc, grid%ht, &
                         grid%pslv_gc, grid%psfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE IF ( ( flag_psfc    .EQ. 1 ) .AND. &
                   ( flag_soilhgt .EQ. 1 ) .AND. &
                   ( config_flags%sfcp_to_sfcp ) ) THEN
            WRITE(a_message,FMT='(A)') 'Using sfcprs2 to compute psfc'
            CALL wrf_message ( a_message )
            CALL sfcprs2(grid%t_gc, grid%qv_gc, grid%ght_gc, grid%psfc_gc, grid%ht, &
                         grid%tavgsfc, grid%p_gc, grid%psfc, we_have_tavgsfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE IF ( flag_slp     .EQ. 1 ) THEN
            WRITE(a_message,FMT='(A)') 'Using sfcprs  to compute psfc'
            CALL wrf_message ( a_message )
            CALL sfcprs (grid%t_gc, grid%qv_gc, grid%ght_gc, grid%pslv_gc, grid%ht, &
                         grid%tavgsfc, grid%p_gc, grid%psfc, we_have_tavgsfc, &
                         ids , ide , jds , jde , 1   , num_metgrid_levels , &
                         ims , ime , jms , jme , 1   , num_metgrid_levels , &
                         its , ite , jts , jte , 1   , num_metgrid_levels )
         ELSE
            WRITE(a_message,FMT='(3(A,I2),A,L1)') 'ERROR in psfc: flag_psfc = ',flag_psfc, &
                                               ', flag_soilhgt = ',flag_soilhgt , &
                                               ', flag_slp = ',flag_slp , &
                                               ', sfcp_to_sfcp = ',config_flags%sfcp_to_sfcp
            CALL wrf_message ( a_message )
            CALL wrf_error_fatal ( 'not enough info for a p sfc computation' )
         END IF

         !  If we have no input surface pressure, we'd better stick something in there.

         IF ( flag_psfc .NE. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
              DO i = its, MIN(ite,ide-1)
                 IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                 grid%psfc_gc(i,j) = grid%psfc(i,j)
                 grid%p_gc(i,1,j) = grid%psfc(i,j)
              END DO
            END DO
         END IF

         !  Integrate the mixing ratio to get the vapor pressure.

         CALL integ_moist ( grid%qv_gc , grid%p_gc , grid%pd_gc , grid%t_gc , grid%ght_gc , grid%intq_gc , &
                            ids , ide , jds , jde , 1   , num_metgrid_levels , &
                            ims , ime , jms , jme , 1   , num_metgrid_levels , &
                            its , ite , jts , jte , 1   , num_metgrid_levels )

         !  If this is UM data, the same moisture removed from the "theta" level pressure data can
         !  be removed from the "rho" level pressures.  This is an approximation.  We'll revisit to
         !  see if this is a bad idea.

         IF ( flag_ptheta .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = num_metgrid_levels-1 , 1 , -1
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     ptemp = ((grid%p_gc(i,k,j) - grid%pd_gc(i,k,j)) + (grid%p_gc(i,k+1,j) - grid%pd_gc(i,k+1,j)))/2
                     grid%pdrho_gc(i,k,j) = grid%prho_gc(i,k,j) - ptemp
                  END DO
               END DO
            END DO
         END IF


         !  Compute the difference between the dry, total surface pressure (input) and the
         !  dry top pressure (constant).

         CALL p_dts ( grid%mu0 , grid%intq_gc , grid%psfc , grid%p_top , &
                      ids , ide , jds , jde , 1   , num_metgrid_levels , &
                      ims , ime , jms , jme , 1   , num_metgrid_levels , &
                      its , ite , jts , jte , 1   , num_metgrid_levels )

         !  Compute the dry, hydrostatic surface pressure.

         CALL p_dhs ( grid%pdhs , grid%ht , p00 , t00 , a , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

         !  Compute the eta levels if not defined already.

         IF ( grid%znw(1) .NE. 1.0 ) THEN
           !DJW Check if any of the domains are going to use vertical
           !nesting with vert_refine_method=2. If so, set vnest as true.
           vnest = .FALSE.
           DO id=1,model_config_rec%max_dom
             IF (model_config_rec%vert_refine_method(id) .EQ. 2) THEN
               vnest = .TRUE.
             ENDIF
           ENDDO
           !DJW If there are eta_levels defined in the namelist and at
           !least one domain is using vertical nesting, then we need to read in
           !the eta_levels.
           IF ((model_config_rec%eta_levels(1) .NE. -1.0) .AND. (vnest)) THEN
             !DJW Added code for specifying multiple domains' eta_levels.
            !First check to make sure that we've not specified more
            !eta_levels than the dimensionality of eta_levels can handle! This
             !issue will most likely cause a break sometime before this
            !check, however it doesn't hurt to include it. To increase max_eta,
            !go to frame/module_driver_constants.F.
             CALL wrf_debug (0, "module_initialize_real: using vert_refine_method=2, reading in eta_levels from namelist.input")
            ks = 0
            DO id=1,grid%id
              ks = ks+model_config_rec%e_vert(id)
            ENDDO
            IF (ks .GT. max_eta) THEN
              CALL wrf_error_fatal("too many vertical levels, increase max_eta in frame/module_driver_constants.F")
            ENDIF
            !Now set the eta_levels to what we specified in the namelist. We've
            !packed all the domains' eta_levels into a 'vector' and now we need
            !to pull only the section of the vector associated with our domain
            !of interest, which is between indicies ks and ke.
            IF (grid%id .EQ. 1) THEN
              ks = 1
              ke = model_config_rec%e_vert(1)
            ELSE
              id = 1
              ks = 1
              ke = 0
              DO WHILE (grid%id .GT. id)
                id = id+1
                ks = ks+model_config_rec%e_vert(id-1)
                ke = ks+model_config_rec%e_vert(id)-1
              ENDDO
            ENDIF
            eta_levels(1:kde) = model_config_rec%eta_levels(ks:ke)
            !Check the value of the first and last eta level for our domain,
            !then check that the vector of eta levels is only decreasing
            IF (eta_levels(1) .NE. 1.0) THEN
               CALL wrf_error_fatal("--- ERROR: the first specified eta_level is not 1.0")
            ENDIF
            IF (eta_levels(kde) .NE. 0.0) THEN
               CALL wrf_error_fatal("--- ERROR: the last specified eta_level is not 0.0")
            ENDIF
            DO k=2,kde
              IF (eta_levels(k) .GT. eta_levels(k-1)) THEN
                 CALL wrf_error_fatal("--- ERROR: specified eta_levels are not uniformly decreasing from 1.0 to 0.0")
              ENDIF
            ENDDO
             !DJW End of added code for specifying eta_levels
           ELSE !We're not using vertical nesting with eta_levels defined for every domain
             !DJW Check if we're doing vertical nesting with integer refinement.
             vnest = .FALSE.
             DO id=1,model_config_rec%max_dom
               IF (model_config_rec%vert_refine_method(id) .EQ. 1) THEN
                 vnest = .TRUE.
               ENDIF
             ENDDO
             !DJW If we're doing vertical nesting using integer refinement and
             !we've got eta_levels specified in the namelist then make sure they are
             !for the parent domain and nothing else.
             IF ((vnest) .AND. (model_config_rec%eta_levels(kde+1) .NE. -1.0)) THEN
               write(wrf_err_message,'(A)') "--- ERROR: too many eta_levels defined in namelist.input."
               CALL wrf_error_fatal( wrf_err_message )
             !DJW Check the value of the first and last eta level for our
             !domain, then check that the vector of eta levels is only decreasing
             ELSEIF ((vnest) .AND. (model_config_rec%eta_levels(1) .NE. -1.0)) THEN
               CALL wrf_debug(0, "module_initialize_real: using vert_refine_method=1, reading in eta_levels for d01 from namelist.input")
             eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
               IF (eta_levels(1) .NE. 1.0) THEN
                 CALL wrf_error_fatal("--- ERROR: the first specified eta_level is not 1.0")
               ENDIF
               IF (eta_levels(kde) .NE. 0.0) THEN
                 CALL wrf_error_fatal("--- ERROR: the last specified eta_level is not 0.0")
               ENDIF
               DO k=2,kde
                 IF (eta_levels(k) .GT. eta_levels(k-1)) THEN
                   CALL wrf_error_fatal("--- ERROR: specified eta_levels are not uniformly decreasing from 1.0 to 0.0")
                 ENDIF
               ENDDO
             ELSE
               !DJW original code to set eta_levels
               eta_levels(1:kde) = model_config_rec%eta_levels(1:kde)
             ENDIF
           ENDIF

            max_dz            = model_config_rec%max_dz
            dzbot             = model_config_rec%dzbot
            dzstretch_s       = model_config_rec%dzstretch_s
            dzstretch_u       = model_config_rec%dzstretch_u
            auto_levels_opt  = model_config_rec%auto_levels_opt

            CALL compute_eta ( grid%znw , auto_levels_opt, &
                               eta_levels , max_eta , max_dz , dzbot , dzstretch_s , dzstretch_u , &
                               grid%p_top , g , p00 , cvpm , a , r_d , cp , &
                               t00 , p1000mb , t0 , tiso , p_strat , a_strat , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte )
         END IF

         !  For vertical coordinate, compute 1d arrays.

         CALL compute_vcoord_1d_coeffs ( grid%ht, grid%etac, grid%znw, &
                                         config_flags%hybrid_opt, &
                                         r_d, g, p1000mb, &
                                         grid%p_top, grid%p00, grid%t00, grid%tlp, &
                                         ids, ide, jds, jde, kds, kde, &
                                         ims, ime, jms, jme, kms, kme, &
                                         its, ite, jts, jte, kts, kte, &
                                         grid%znu, &
                                         grid%c1f, grid%c2f, grid%c3f, grid%c4f, &
                                         grid%c1h, grid%c2h, grid%c3h, grid%c4h )

         IF ( config_flags%interp_theta ) THEN

            !  The input field is temperature, we want potential temp.

            CALL t_to_theta ( grid%t_gc , grid%p_gc , p00 , &
                              ids , ide , jds , jde , 1   , num_metgrid_levels , &
                              ims , ime , jms , jme , 1   , num_metgrid_levels , &
                              its , ite , jts , jte , 1   , num_metgrid_levels )
         END IF

         IF ( flag_slp .EQ. 1 ) THEN

            !  On the eta surfaces, compute the dry pressure = mu eta, stored in
            !  grid%pb, since it is a pressure, and we don't need another kms:kme 3d
            !  array floating around.  The grid%pb array is re-computed as the base pressure
            !  later after the vertical interpolations are complete.

            CALL p_dry ( grid%mu0 , grid%znw , grid%p_top , grid%pb , want_full_levels , &
                         grid%c3f , grid%c3h , grid%c4f , grid%c4h ,  &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte )

            !  All of the vertical interpolations are done in dry-pressure space.  The
            !  input data has had the moisture removed (grid%pd_gc).  The target levels (grid%pb)
            !  had the vapor pressure removed from the surface pressure, then they were
            !  scaled by the eta levels.

            interp_type = 2
            lagrange_order = grid%lagrange_order
            linear_interp = grid%linear_interp
            lowest_lev_from_sfc = .FALSE.
            use_levels_below_ground = .TRUE.
            use_surface = .TRUE.
            zap_close_levels = grid%zap_close_levels
            force_sfc_in_vinterp = 0
            t_extrap_type = grid%t_extrap_type
            extrap_type = 1

            !  For the height field, the lowest level pressure is the slp (approximately "dry").  The
            !  lowest level of the input height field (to be associated with slp) then is an array
            !  of zeros.

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  grid%psfc_gc(i,j) = grid%pd_gc(i,1,j)
                  grid%pd_gc(i,1,j) = grid%pslv_gc(i,j) - ( grid%p_gc(i,1,j) - grid%pd_gc(i,1,j) )
                  grid%ht_gc(i,j) = grid%ght_gc(i,1,j)
                  grid%ght_gc(i,1,j) = 0.
               END DO
            END DO

#ifdef DM_PARALLEL
         ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte

         !  Stencil for pressure is required for the pressure difference for the max_wind
         !  and trop level data.

# include "HALO_EM_VINTERP_UV_1.inc"
#endif

            CALL vert_interp ( grid%ght_gc , grid%pd_gc , grid%ph0 , grid%pb , &
                               grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                               grid%pmaxwnn , grid%ptropnn , &
                               flag_hgtmaxw , flag_hgttrop , &
                               config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                               config_flags%maxw_above_this_level , &
                               num_metgrid_levels , 'Z' , &
                               interp_type , lagrange_order , extrap_type , &
                               lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                               zap_close_levels , force_sfc_in_vinterp , grid%id , &
                               ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte )

            !  Put things back to normal.

            DO j = jts, MIN(jte,jde-1)
               DO i = its, MIN(ite,ide-1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%pd_gc(i,1,j) = grid%psfc_gc(i,j)
                  grid%ght_gc(i,1,j) = grid%ht_gc(i,j)
               END DO
            END DO

         END IF

         !  Now the rest of the variables on half-levels to inteprolate.

         CALL p_dry ( grid%mu0 , grid%znw , grid%p_top , grid%pb , want_half_levels , &
                      grid%c3f , grid%c3h , grid%c4f , grid%c4h ,  &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

         interp_type = grid%interp_type
         lagrange_order = grid%lagrange_order
         lowest_lev_from_sfc = grid%lowest_lev_from_sfc
         use_levels_below_ground = grid%use_levels_below_ground
         use_surface = grid%use_surface
         zap_close_levels = grid%zap_close_levels
         force_sfc_in_vinterp = grid%force_sfc_in_vinterp
         t_extrap_type = grid%t_extrap_type
         extrap_type = grid%extrap_type

#ifdef DM_PARALLEL
         ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte

         !  Stencil for pressure is required for the pressure difference for the max_wind
         !  and trop level data.

# include "HALO_EM_VINTERP_UV_1.inc"
#endif

         !  Interpolate RH, diagnose Qv later when have temp and pressure.  Temporarily
         !  store this in the u_1 space, for later diagnosis into Qv and stored into moist.

         CALL vert_interp ( grid%rh_gc , grid%pd_gc , grid%u_1 , grid%pb , &
                            grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                            grid%pmaxwnn , grid%ptropnn , &
                            0 , 0 , &
                            config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                            config_flags%maxw_above_this_level , &
                            num_metgrid_levels , 'Q' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , grid%id , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

         !  If this is theta being interpolated, AND we have extra levels for temperature,
         !  convert those extra levels (trop and max wind) to potential temp.

         IF ( ( config_flags%interp_theta ) .AND. ( flag_tmaxw .EQ. 1 ) ) THEN
            CALL t_to_theta ( grid%tmaxw , grid%pmaxw , p00 , &
                              ids , ide , jds , jde , 1 , 1 , &
                              ims , ime , jms , jme , 1 , 1 , &
                              its , ite , jts , jte , 1 , 1 )
         END IF

         IF ( ( config_flags%interp_theta ) .AND. ( flag_ttrop .EQ. 1 ) ) THEN
            CALL t_to_theta ( grid%ttrop , grid%ptrop , p00 , &
                              ids , ide , jds , jde , 1 , 1 , &
                              ims , ime , jms , jme , 1 , 1 , &
                              its , ite , jts , jte , 1 , 1 )
         END IF

         !  Depending on the setting of interp_theta = T/F, t_gc is is either theta Xor
         !  temperature, and that means that the t_2 field is also the associated field.
         !  It is better to interpolate temperature and potential temperature in LOG(p),
         !  regardless of requested default.

         interp_type = 2
         CALL vert_interp ( grid%t_gc , grid%pd_gc , grid%t_2               , grid%pb , &
                            grid%tmaxw , grid%ttrop , grid%pmaxw , grid%ptrop , &
                            grid%pmaxwnn , grid%ptropnn , &
                            flag_tmaxw , flag_ttrop , &
                            config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                            config_flags%maxw_above_this_level , &
                            num_metgrid_levels , 'T' , &
                            interp_type , lagrange_order , t_extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , grid%id , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
         interp_type = grid%interp_type
     
         !  It is better to interpolate pressure in p regardless of the default options

         interp_type = 1
         CALL vert_interp ( grid%p_gc , grid%pd_gc , grid%p               , grid%pb , &
                            grid%pmaxw , grid%ptrop , grid%pmaxw , grid%ptrop , &
                            grid%pmaxwnn , grid%ptropnn , &
                            flag_pmaxw , flag_ptrop , &
                            config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                            config_flags%maxw_above_this_level , &
                            num_metgrid_levels , 'T' , &
                            interp_type , lagrange_order , t_extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , grid%id , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )
         interp_type = grid%interp_type

         !  Do not have full pressure on eta levels, get a first guess at Qv by using
         !  dry pressure.  The use of u_1 (rh) and v_1 (temperature) is temporary.
         !  We fix the approximation to Qv after the total pressure is available on
         !  eta surfaces.

         grid%v_1 = grid%t_2

         IF ( config_flags%interp_theta ) THEN
            CALL theta_to_t ( grid%v_1 , grid%p  , p00 , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte )
         END IF

         IF      ( config_flags%rh2qv_method .eq. 1 ) THEN
            CALL rh_to_mxrat1(grid%u_1, grid%v_1, grid%p , moist(:,:,:,P_QV) ,       &
                              config_flags%rh2qv_wrt_liquid ,                        &
                              config_flags%qv_max_p_safe ,                           &
                              config_flags%qv_max_flag , config_flags%qv_max_value , &
                              config_flags%qv_min_p_safe ,                           &
                              config_flags%qv_min_flag , config_flags%qv_min_value , &
                              ids , ide , jds , jde , kds , kde ,                    &
                              ims , ime , jms , jme , kms , kme ,                    &
                              its , ite , jts , jte , kts , kte-1 )
         ELSE IF ( config_flags%rh2qv_method .eq. 2 ) THEN
            CALL rh_to_mxrat2(grid%u_1, grid%v_1, grid%p , moist(:,:,:,P_QV) ,       &
                              config_flags%rh2qv_wrt_liquid ,                        &
                              config_flags%qv_max_p_safe ,                           &
                              config_flags%qv_max_flag , config_flags%qv_max_value , &
                              config_flags%qv_min_p_safe ,                           &
                              config_flags%qv_min_flag , config_flags%qv_min_value , &
                              ids , ide , jds , jde , kds , kde ,                    &
                              ims , ime , jms , jme , kms , kme ,                    &
                              its , ite , jts , jte , kts , kte-1 )
         END IF

         IF ( .NOT. config_flags%interp_theta ) THEN
            CALL t_to_theta ( grid%t_2 , grid%p , p00 , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte-1 )
         END IF

         num_3d_m = num_moist
         num_3d_s = num_scalar

         IF ( flag_qr .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QR ) THEN
                  CALL vert_interp ( grid%qr_gc , grid%pd_gc , moist(:,:,:,P_QR) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( ( flag_qc .EQ. 1 ) .OR. ( flag_speccldl .EQ. 1 ) ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QC ) THEN
                  IF ( flag_speccldl .EQ. 1 ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO k = 1 , num_metgrid_levels
                           DO i = its, MIN(ite,ide-1)
                              IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                              grid%qc_gc(i,k,j) = grid%cl_gc(i,k,j) /( 1. - grid%cl_gc(i,k,j) )
                           END DO
                        END DO
                     END DO
                  END IF
                  CALL vert_interp ( grid%qc_gc , grid%pd_gc , moist(:,:,:,P_QC) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( ( flag_qi .EQ. 1 ) .OR. ( flag_speccldf .EQ. 1 ) ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QI ) THEN
                  IF ( flag_speccldf .EQ. 1 ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO k = 1 , num_metgrid_levels
                           DO i = its, MIN(ite,ide-1)
                              IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                              grid%qi_gc(i,k,j) = grid%cf_gc(i,k,j) /( 1. - grid%cf_gc(i,k,j) )
                           END DO
                        END DO
                     END DO
                  END IF
                  CALL vert_interp ( grid%qi_gc , grid%pd_gc , moist(:,:,:,P_QI) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qs .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QS ) THEN
                  CALL vert_interp ( grid%qs_gc , grid%pd_gc , moist(:,:,:,P_QS) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qg .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QG ) THEN
                  CALL vert_interp ( grid%qg_gc , grid%pd_gc , moist(:,:,:,P_QG) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qh .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_m
               IF ( im .EQ. P_QH ) THEN
                  CALL vert_interp ( grid%qh_gc , grid%pd_gc , moist(:,:,:,P_QH) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

         IF ( flag_qni .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNI ) THEN
                  CALL vert_interp ( grid%qni_gc , grid%pd_gc , scalar(:,:,:,P_QNI) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
    
         IF ( flag_qnc .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNC ) THEN
                  CALL vert_interp ( grid%qnc_gc , grid%pd_gc , scalar(:,:,:,P_QNC) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
    
         IF ( flag_qnr .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNR ) THEN
                  CALL vert_interp ( grid%qnr_gc , grid%pd_gc , scalar(:,:,:,P_QNR) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
    
         IF ( flag_qns .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNS ) THEN
                  CALL vert_interp ( grid%qns_gc , grid%pd_gc , scalar(:,:,:,P_QNS) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
    
         IF ( flag_qng .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNG ) THEN
                  CALL vert_interp ( grid%qng_gc , grid%pd_gc , scalar(:,:,:,P_QNG) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF
    
         IF ( flag_qnh .EQ. 1 ) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNH ) THEN
                  CALL vert_interp ( grid%qnh_gc , grid%pd_gc , scalar(:,:,:,P_QNH) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
               END IF
            END DO
         END IF

!..If we have number of water-friendly aerosols from monthly climo data, interpolate to WRF model levels.

         IF ( flag_qnwfa .EQ. 1 .and. (config_flags%use_aero_icbc.or.config_flags%use_rap_aero_icbc)) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNWFA ) THEN
                if(config_flags%use_rap_aero_icbc) then
!HRRR - aerosol input from WPS
                  CALL wrf_debug ( 0 , 'Using Thompson_aerosol_aware so vertically-interpolating QNWFA from WPS data to fill scalar')
                  CALL vert_interp ( grid%qnwfa_gc , grid%pd_gc , scalar(:,:,:,P_QNWFA) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )

                else
                  CALL wrf_debug ( 0 , 'Using Thompson_aerosol_aware so vertically-interpolating QNWFA monthly climo arrays to fill scalar')
                  CALL vert_interp ( grid%QNWFA_now , grid%pd_gc , scalar(:,:,:,P_QNWFA) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
                endif
               END IF
            END DO
         ELSEIF ( flag_qnwfa .EQ. 1 .and. (.NOT. config_flags%use_aero_icbc)) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNWFA ) THEN
                  DO j = jts, MIN(jte,jde-1)
                     DO k = kts, kte
                        DO i = its, MIN(ite,ide-1)
                           scalar(i,k,j,P_QNWFA) = 0.
                        END DO
                     END DO
                  END DO
               END IF
            END DO
         END IF

!..If we have number of ice-friendly aerosols from monthly climo data, interpolate to WRF model levels.

         IF ( flag_qnifa .EQ. 1 .and. (config_flags%use_aero_icbc.or.config_flags%use_rap_aero_icbc)) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNIFA ) THEN
                if(config_flags%use_rap_aero_icbc) then
! HRRR - aerosol input from WPS
                  CALL vert_interp ( grid%qnifa_gc , grid%pd_gc , scalar(:,:,:,P_QNIFA) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )

                else
                  CALL vert_interp ( grid%QNIFA_now , grid%pd_gc , scalar(:,:,:,P_QNIFA) , grid%pb , &
                                     grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                     grid%pmaxwnn , grid%ptropnn , &
                                     0 , 0 , &
                                     config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                     config_flags%maxw_above_this_level , &
                                     num_metgrid_levels , 'Q' , &
                                     interp_type , linear_interp , extrap_type , &
                                     .false. , use_levels_below_ground , use_surface , &
                                     zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                     ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte )
                endif
               END IF
            END DO
         ELSEIF ( flag_qnifa .EQ. 1 .and. (.NOT. config_flags%use_aero_icbc)) THEN
            DO im = PARAM_FIRST_SCALAR, num_3d_s
               IF ( im .EQ. P_QNIFA ) THEN
                  DO j = jts, MIN(jte,jde-1)
                     DO k = kts, kte
                        DO i = its, MIN(ite,ide-1)
                           scalar(i,k,j,P_QNIFA) = 0.
                        END DO
                     END DO
                  END DO
               END IF
            END DO
         END IF

!=========================================================================================
!    START OF OPTIONAL 3D DATA, USUALLY AEROSOLS
!=========================================================================================

#if ( WRF_CHEM == 1 )
         !  Do we have the old data that came in on the same vertical levels as the other
         !  met variables? If so, we can skip all of this interpolation, as the pressure field
         !  is allocated, but all zeros.
 
         IF ( config_flags%gca_input_opt .EQ. 1 ) THEN
         IF ( ( config_flags%num_gca_levels .GT. 0 ) .AND. &
              ( ABS(grid %p_gca(its,config_flags%num_gca_levels/2,jts)) .GT. 1 ) ) THEN

            !  Insert source code here to vertically interpolate an extra set of 3d arrays
            !  that could be on a different vertical structure than the input atmospheric
            !  data.  Mostly, this is expected to be for monthly data (such as background
            !  aerosol information).

            ! OPTIONAL DATA #1: GCA - Go Cart Aerosols: OH, H2O2, NO3
            !       Pressure name: p_gca
            !       Number of vertical levels: num_gca_levels
            !       Variable names (assumed to end with _jan, _feb, _mar, ..., _nov, _dec): oh, h2o2, no3
            !       Option to interpolate data: gca_input_opt = 1
            !       Not stored in scalar arrays.

            IF ( config_flags%gca_input_opt .EQ. 1 ) THEN

               CALL wrf_debug ( 0 , 'Using monthly GOcart Aerosol input: OH, H2O2, NO3 from metgrid input file' )

               !  There are three fields - they are 3d, so no easy way to loop over them.
               !  OH - Hydroxyl
               !  H2O2 - Hydrogen Peroxide
               !  NO3 - Nitrate

               DO k = 1, config_flags%num_gca_levels
                  WRITE(a_message,*) '  transferring each K-level ', k, ' to   OH, sample Jan data, ', grid %  oh_gca_jan(its,k,jts)
                  CALL wrf_debug ( 1 , a_message)
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid%qntemp(i, 1, j) = grid %  oh_gca_jan(i,k,j)
                        grid%qntemp(i, 2, j) = grid %  oh_gca_feb(i,k,j)
                        grid%qntemp(i, 3, j) = grid %  oh_gca_mar(i,k,j)
                        grid%qntemp(i, 4, j) = grid %  oh_gca_apr(i,k,j)
                        grid%qntemp(i, 5, j) = grid %  oh_gca_may(i,k,j)
                        grid%qntemp(i, 6, j) = grid %  oh_gca_jun(i,k,j)
                        grid%qntemp(i, 7, j) = grid %  oh_gca_jul(i,k,j)
                        grid%qntemp(i, 8, j) = grid %  oh_gca_aug(i,k,j)
                        grid%qntemp(i, 9, j) = grid %  oh_gca_sep(i,k,j)
                        grid%qntemp(i,10, j) = grid %  oh_gca_oct(i,k,j)
                        grid%qntemp(i,11, j) = grid %  oh_gca_nov(i,k,j)
                        grid%qntemp(i,12, j) = grid %  oh_gca_dec(i,k,j)
                     END DO
                  END DO
                  IF ( k .EQ. 1 ) THEN
                     WRITE(a_message,*) ' GOcart Aerosols   OH (jan and feb) ', grid%qntemp(its,1,jts),grid%qntemp(its,2,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                                ids , ide , jds , jde , kds , kde , &
                                                ims , ime , jms , jme , kms , kme , &
                                                its , ite , jts , jte , kts , kte )
                  IF ( k .eq. 1 ) THEN
                     write(a_message,*) ' GOcart Aerosols   OH (now) ', grid%qntemp2(its,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid %  oh_gca_now(i,k,j) = grid%qntemp2(i,j)
                     END DO
                  END DO
               END DO

               CALL vert_interp ( grid %  oh_gca_now , grid%p_gca , grid%backg_oh   , grid%pb , &
                                  grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                  grid%pmaxwnn , grid%ptropnn , &
                                  0 , 0 , &
                                  config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                  config_flags%maxw_above_this_level , &
                                  config_flags%num_gca_levels , 'Q' , &
                                  interp_type , linear_interp , extrap_type , &
                                  .false. , use_levels_below_ground , use_surface , &
                                  zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte )

               DO k = 1, config_flags%num_gca_levels
                  WRITE(a_message,*) '  transferring each K-level ', k, ' to H2O2, sample Jan data, ', grid %h2o2_gca_jan(its,k,jts)
                  CALL wrf_debug ( 1 , a_message)
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid%qntemp(i, 1, j) = grid %h2o2_gca_jan(i,k,j)
                        grid%qntemp(i, 2, j) = grid %h2o2_gca_feb(i,k,j)
                        grid%qntemp(i, 3, j) = grid %h2o2_gca_mar(i,k,j)
                        grid%qntemp(i, 4, j) = grid %h2o2_gca_apr(i,k,j)
                        grid%qntemp(i, 5, j) = grid %h2o2_gca_may(i,k,j)
                        grid%qntemp(i, 6, j) = grid %h2o2_gca_jun(i,k,j)
                        grid%qntemp(i, 7, j) = grid %h2o2_gca_jul(i,k,j)
                        grid%qntemp(i, 8, j) = grid %h2o2_gca_aug(i,k,j)
                        grid%qntemp(i, 9, j) = grid %h2o2_gca_sep(i,k,j)
                        grid%qntemp(i,10, j) = grid %h2o2_gca_oct(i,k,j)
                        grid%qntemp(i,11, j) = grid %h2o2_gca_nov(i,k,j)
                        grid%qntemp(i,12, j) = grid %h2o2_gca_dec(i,k,j)
                     END DO
                  END DO
                  IF ( k .EQ. 1 ) THEN
                     WRITE(a_message,*) ' GOcart Aerosols H2O2 (jan and feb) ', grid%qntemp(its,1,jts),grid%qntemp(its,2,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                                ids , ide , jds , jde , kds , kde , &
                                                ims , ime , jms , jme , kms , kme , &
                                                its , ite , jts , jte , kts , kte )
                  IF ( k .eq. 1 ) THEN
                     write(a_message,*) ' GOcart Aerosols H2O2 (now) ', grid%qntemp2(its,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid %h2o2_gca_now(i,k,j) = grid%qntemp2(i,j)
                     END DO
                  END DO
               END DO

               CALL vert_interp ( grid %h2o2_gca_now , grid%p_gca , grid%backg_h2o2 , grid%pb , &
                                  grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                  grid%pmaxwnn , grid%ptropnn , &
                                  0 , 0 , &
                                  config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                  config_flags%maxw_above_this_level , &
                                  config_flags%num_gca_levels , 'Q' , &
                                  interp_type , linear_interp , extrap_type , &
                                  .false. , use_levels_below_ground , use_surface , &
                                  zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte )

               DO k = 1, config_flags%num_gca_levels
                  WRITE(a_message,*) '  transferring each K-level ', k, ' to  NO3, sample Jan data, ', grid % no3_gca_jan(its,k,jts)
                  CALL wrf_debug ( 1 , a_message)
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid%qntemp(i, 1, j) = grid % no3_gca_jan(i,k,j)
                        grid%qntemp(i, 2, j) = grid % no3_gca_feb(i,k,j)
                        grid%qntemp(i, 3, j) = grid % no3_gca_mar(i,k,j)
                        grid%qntemp(i, 4, j) = grid % no3_gca_apr(i,k,j)
                        grid%qntemp(i, 5, j) = grid % no3_gca_may(i,k,j)
                        grid%qntemp(i, 6, j) = grid % no3_gca_jun(i,k,j)
                        grid%qntemp(i, 7, j) = grid % no3_gca_jul(i,k,j)
                        grid%qntemp(i, 8, j) = grid % no3_gca_aug(i,k,j)
                        grid%qntemp(i, 9, j) = grid % no3_gca_sep(i,k,j)
                        grid%qntemp(i,10, j) = grid % no3_gca_oct(i,k,j)
                        grid%qntemp(i,11, j) = grid % no3_gca_nov(i,k,j)
                        grid%qntemp(i,12, j) = grid % no3_gca_dec(i,k,j)
                     END DO
                  END DO
                  IF ( k .EQ. 1 ) THEN
                     WRITE(a_message,*) ' GOcart Aerosols  NO3 (jan and feb) ', grid%qntemp(its,1,jts),grid%qntemp(its,2,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                                ids , ide , jds , jde , kds , kde , &
                                                ims , ime , jms , jme , kms , kme , &
                                                its , ite , jts , jte , kts , kte )
                  IF ( k .eq. 1 ) THEN
                     write(a_message,*) ' GOcart Aerosols  NO3 (now) ', grid%qntemp2(its,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid % no3_gca_now(i,k,j) = grid%qntemp2(i,j)
                     END DO
                  END DO
               END DO

               CALL vert_interp ( grid % no3_gca_now , grid%p_gca , grid%backg_no3 , grid%pb , &
                                  grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                  grid%pmaxwnn , grid%ptropnn , &
                                  0 , 0 , &
                                  config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                  config_flags%maxw_above_this_level , &
                                  config_flags%num_gca_levels , 'Q' , &
                                  interp_type , linear_interp , extrap_type , &
                                  .false. , use_levels_below_ground , use_surface , &
                                  zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte )
            END IF
            END IF
         END IF
#endif

         !  Do we have the old data that came in on the same vertical levels as the other
         !  met variables? If so, we can skip all of this interpolation, as the pressure field
         !  is allocated, but all zeros.
 
         IF ( config_flags%wif_input_opt .EQ. 1 ) THEN
         IF ( ( config_flags%num_wif_levels .GT. 0 ) .AND. &
              ( ABS(grid %p_wif_jan(its,config_flags%num_wif_levels/2,jts)) .GT. 1 ) ) THEN

            ! OPTIONAL DATA #2: Thompson Water-Friendly Ice-Friendly Aerosols
            !       Pressure name (assumed to end with _jan, _feb, _mar, ..., _nov, _dec): p_wif
            !       Number of vertical levels: num_wif_levels
            !       Variable names (assumed to end with _jan, _feb, _mar, ..., _nov, _dec): w_wif, i_wif
            !       Option to interpolate data: wif_input_opt = 1
            !       Stored in scalar arrays, tested and assumed to be upside down.

            IF ( ( config_flags%wif_input_opt .EQ. 1 ) .AND. &
                 ( config_flags%mp_physics .EQ. THOMPSONAERO ) .AND. &
                 ( flag_qnwfa .EQ. 1 ) .AND.  &
                 ( flag_qnifa .EQ. 1 ) ) THEN

               CALL wrf_debug ( 0 , 'Using monthly Water-Friendly and Ice-Friendly aerosols from metgrid input file' )

               !  There are two data fields plus pressure - they are 3d, so no easy way to loop over them.
               !  QNWFA - Number concentration water-friendly aerosols
               !  QNIFA - Number concentration ice-friendly aerosols

               !  First, get the pressure temporally interpolated to the correct date/time since
               !  this is a hybrid coordinate (not isobaric), and the pressure changes by month.
               !  NOTE: The input pressure is not vertically interpolated, but the other two input   
               !  fields (QNWFA, QNIFA) are interpolated to the WRF eta coordinate.

               IF ( grid%p_wif_jan(its,config_flags%num_wif_levels/2-1,jts) - &
                    grid%p_wif_jan(its,config_flags%num_wif_levels/2+1,jts) .LT. 0 ) THEN
                  wif_upside_down = .TRUE.
               END IF

               DO k = 1, config_flags%num_wif_levels
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid%qntemp(i, 1, j) = grid %p_wif_jan(i,k,j)
                        grid%qntemp(i, 2, j) = grid %p_wif_feb(i,k,j)
                        grid%qntemp(i, 3, j) = grid %p_wif_mar(i,k,j)
                        grid%qntemp(i, 4, j) = grid %p_wif_apr(i,k,j)
                        grid%qntemp(i, 5, j) = grid %p_wif_may(i,k,j)
                        grid%qntemp(i, 6, j) = grid %p_wif_jun(i,k,j)
                        grid%qntemp(i, 7, j) = grid %p_wif_jul(i,k,j)
                        grid%qntemp(i, 8, j) = grid %p_wif_aug(i,k,j)
                        grid%qntemp(i, 9, j) = grid %p_wif_sep(i,k,j)
                        grid%qntemp(i,10, j) = grid %p_wif_oct(i,k,j)
                        grid%qntemp(i,11, j) = grid %p_wif_nov(i,k,j)
                        grid%qntemp(i,12, j) = grid %p_wif_dec(i,k,j)
                     END DO
                  END DO
                  CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                                ids , ide , jds , jde , kds , kde , &
                                                ims , ime , jms , jme , kms , kme , &
                                                its , ite , jts , jte , kts , kte )
                  IF      (       wif_upside_down ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO i = its, MIN(ite,ide-1)
                           grid %p_wif_now(i,config_flags%num_wif_levels+1-k,j) = grid%qntemp2(i,j)
                        END DO
                     END DO
                  ELSE IF ( .NOT. wif_upside_down ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO i = its, MIN(ite,ide-1)
                           grid %p_wif_now(i,                              k,j) = grid%qntemp2(i,j)
                        END DO
                     END DO
                  END IF
               END DO

               DO k = 1, config_flags%num_wif_levels
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid%qntemp(i, 1, j) = grid %w_wif_jan(i,k,j)
                        grid%qntemp(i, 2, j) = grid %w_wif_feb(i,k,j)
                        grid%qntemp(i, 3, j) = grid %w_wif_mar(i,k,j)
                        grid%qntemp(i, 4, j) = grid %w_wif_apr(i,k,j)
                        grid%qntemp(i, 5, j) = grid %w_wif_may(i,k,j)
                        grid%qntemp(i, 6, j) = grid %w_wif_jun(i,k,j)
                        grid%qntemp(i, 7, j) = grid %w_wif_jul(i,k,j)
                        grid%qntemp(i, 8, j) = grid %w_wif_aug(i,k,j)
                        grid%qntemp(i, 9, j) = grid %w_wif_sep(i,k,j)
                        grid%qntemp(i,10, j) = grid %w_wif_oct(i,k,j)
                        grid%qntemp(i,11, j) = grid %w_wif_nov(i,k,j)
                        grid%qntemp(i,12, j) = grid %w_wif_dec(i,k,j)
                     END DO
                  END DO
                  CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                                ids , ide , jds , jde , kds , kde , &
                                                ims , ime , jms , jme , kms , kme , &
                                                its , ite , jts , jte , kts , kte )
                  IF      (       wif_upside_down ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO i = its, MIN(ite,ide-1)
                           grid %w_wif_now(i,config_flags%num_wif_levels+1-k,j) = grid%qntemp2(i,j)
                        END DO
                     END DO
                  ELSE IF ( .NOT. wif_upside_down ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO i = its, MIN(ite,ide-1)
                           grid %w_wif_now(i,                              k,j) = grid%qntemp2(i,j)
                        END DO
                     END DO
                  END IF
               END DO

               !..Capture the monthly climatology value for the lowest level because we use later to compute surface flux value.

               DO j = jts, MIN(jte,jde-1)
                  DO i = its, MIN(ite,ide-1)
                     grid%QNWFA_now(i,1,j) = grid%w_wif_now(i,1,j)
                  END DO
               END DO

               CALL vert_interp ( grid %w_wif_now , grid%p_wif_now , scalar(:,:,:,P_qnwfa) , grid%pb , &
                                  grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                  grid%pmaxwnn , grid%ptropnn , &
                                  0 , 0 , &
                                  config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                  config_flags%maxw_above_this_level , &
                                  config_flags%num_wif_levels , 'Q' , &
                                  interp_type , linear_interp , extrap_type , &
                                  .false. , use_levels_below_ground , use_surface , &
                                  zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte )


               DO k = 1, config_flags%num_wif_levels
                  WRITE(a_message,*) '  transferring each K-level ', k, ' to QNIFA, sample Jan data, ', grid %i_wif_jan(its,k,jts)
                  CALL wrf_debug ( 1 , a_message)
                  DO j = jts, MIN(jte,jde-1)
                     DO i = its, MIN(ite,ide-1)
                        grid%qntemp(i, 1, j) = grid %i_wif_jan(i,k,j)
                        grid%qntemp(i, 2, j) = grid %i_wif_feb(i,k,j)
                        grid%qntemp(i, 3, j) = grid %i_wif_mar(i,k,j)
                        grid%qntemp(i, 4, j) = grid %i_wif_apr(i,k,j)
                        grid%qntemp(i, 5, j) = grid %i_wif_may(i,k,j)
                        grid%qntemp(i, 6, j) = grid %i_wif_jun(i,k,j)
                        grid%qntemp(i, 7, j) = grid %i_wif_jul(i,k,j)
                        grid%qntemp(i, 8, j) = grid %i_wif_aug(i,k,j)
                        grid%qntemp(i, 9, j) = grid %i_wif_sep(i,k,j)
                        grid%qntemp(i,10, j) = grid %i_wif_oct(i,k,j)
                        grid%qntemp(i,11, j) = grid %i_wif_nov(i,k,j)
                        grid%qntemp(i,12, j) = grid %i_wif_dec(i,k,j)
                     END DO
                  END DO
                  IF ( k .EQ. 1 ) THEN
                     WRITE(a_message,*) ' QNIFA (jan and feb) ', grid%qntemp(its,1,jts),grid%qntemp(its,2,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  CALL monthly_interp_to_date ( grid%qntemp , current_date , grid%qntemp2 , &
                                                ids , ide , jds , jde , kds , kde , &
                                                ims , ime , jms , jme , kms , kme , &
                                                its , ite , jts , jte , kts , kte )
                  IF ( k .eq. 1 ) THEN
                     write(a_message,*) ' QNIFA (now) ', grid%qntemp2(its,jts)
                     CALL wrf_debug ( 1 , a_message)
                  END IF
                  IF      (       wif_upside_down ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO i = its, MIN(ite,ide-1)
                           grid %i_wif_now(i,config_flags%num_wif_levels+1-k,j) = grid%qntemp2(i,j)
                        END DO
                     END DO
                  ELSE IF ( .NOT. wif_upside_down ) THEN
                     DO j = jts, MIN(jte,jde-1)
                        DO i = its, MIN(ite,ide-1)
                           grid %i_wif_now(i,                              k,j) = grid%qntemp2(i,j)
                        END DO
                     END DO
                  END IF
               END DO

               CALL vert_interp ( grid %i_wif_now , grid%p_wif_now , scalar(:,:,:,P_qnifa) , grid%pb , &
                                  grid%hgtmaxw , grid%hgttrop , grid%pmaxw , grid%ptrop , &
                                  grid%pmaxwnn , grid%ptropnn , &
                                  0 , 0 , &
                                  config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                                  config_flags%maxw_above_this_level , &
                                  config_flags%num_wif_levels , 'Q' , &
                                  interp_type , linear_interp , extrap_type , &
                                  .false. , use_levels_below_ground , use_surface , &
                                  zap_close_levels , force_sfc_in_vinterp , grid%id , &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte )

            ELSE IF ( ( config_flags%mp_physics .EQ. THOMPSONAERO  ) .AND. &
                      ( ( config_flags%wif_input_opt .EQ. USE_WIF_INPUT ) .OR.  &
                        ( config_flags%use_aero_icbc                    ) ) .AND. &
                      ( ( flag_qnwfa .NE. 1 ) .OR.  &
                        ( flag_qnifa .NE. 1 ) ) ) THEN
               WRITE (a_message,*) "--- ERROR: QNWFA or QNIFA fields are not in the metgrid input, they can't be interpolated"
               CALL wrf_message ( a_message)
               WRITE (a_message,*) '--- ERROR: See https://www2.mmm.ucar.edu/wrf/users/wrfv3.9/mp28_updated.html'
               CALL wrf_message ( a_message)
               WRITE (a_message,*) '--- ERROR: Neither wif_input_opt=1 nor use_aero_icbc=T are allowed without aerosol data'
               CALL wrf_error_fatal ( a_message)

            END IF
            END IF
         END IF

!=========================================================================================
!    END OF OPTIONAL 3D DATA, USUALLY AEROSOLS
!=========================================================================================

         !  If this is UM data, put the dry rho-based pressure back into the dry pressure array.
         !  Since the dry pressure is no longer needed, no biggy.

         IF ( flag_ptheta .EQ. 1 ) THEN
            DO j = jts, MIN(jte,jde-1)
               DO k = 1 , num_metgrid_levels
                  DO i = its, MIN(ite,ide-1)
                     IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     grid%pd_gc(i,k,j) = grid%prho_gc(i,k,j)
                  END DO
               END DO
            END DO
         END IF

#ifdef DM_PARALLEL
         ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte

         !  For the U and V vertical interpolation, we need the pressure defined
         !  at both the locations for the horizontal momentum, which we get by
         !  averaging two pressure values (i and i-1 for U, j and j-1 for V).  The
         !  pressure field on input (grid%pd_gc) and the pressure of the new coordinate
         !  (grid%pb) would only need an 8 point stencil.  However, the i+1 i-1 and
         !  j+1 j-1 for the pressure difference for the max_wind and trop level data
         !  require an 8 stencil for all of the mass point variables and a 24-point
         !  stencil for U and V.

# include "HALO_EM_VINTERP_UV_1.inc"
#endif

         CALL vert_interp ( grid%u_gc , grid%pd_gc , grid%u_2               , grid%pb , &
                            grid%umaxw , grid%utrop , grid%pmaxw , grid%ptrop , &
                            grid%pmaxwnn , grid%ptropnn , &
                            flag_umaxw , flag_utrop , &
                            config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                            config_flags%maxw_above_this_level , &
                            num_metgrid_levels , 'U' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , grid%id , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

         CALL vert_interp ( grid%v_gc , grid%pd_gc , grid%v_2               , grid%pb , &
                            grid%vmaxw , grid%vtrop , grid%pmaxw , grid%ptrop , &
                            grid%pmaxwnn , grid%ptropnn , &
                            flag_vmaxw , flag_vtrop , &
                            config_flags%maxw_horiz_pres_diff , config_flags%trop_horiz_pres_diff , &
                            config_flags%maxw_above_this_level , &
                            num_metgrid_levels , 'V' , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , grid%id , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      END IF     !   <----- END OF VERTICAL INTERPOLATION PART ---->

      ! Set the temperature of the inland lakes to tavgsfc if the temperature is available
      ! and islake is > num_veg_cat

      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL nl_get_islake  ( grid%id , grid%islake )


      IF ( grid%islake < 0 ) THEN
         grid%lakeflag=0
         CALL wrf_debug ( 0 , 'Old data, no inland lake information')

            DO j=jts,MIN(jde-1,jte)
               DO i=its,MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  IF ( ( ( grid%landusef(i,grid%iswater,j) >= 0.5 ) .OR. ( grid%lu_index(i,j) == grid%iswater ) ) .AND. &
                       ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) ) THEN
                     IF ( we_have_tavgsfc ) THEN
                        grid%sst(i,j) = grid%tavgsfc(i,j)
                     END IF
                     IF ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) THEN
                        grid%sst(i,j) = grid%tsk(i,j)
                     END IF
                     IF ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) THEN
                        grid%sst(i,j) = grid%t2(i,j)
                     END IF
                  END IF
               END DO
            END DO
      ELSE
         grid%lakeflag=1
         IF ( we_have_tavgsfc ) THEN

            CALL wrf_debug ( 0 , 'Using inland lakes with average surface temperature')
            DO j=jts,MIN(jde-1,jte)
               DO i=its,MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  IF ( ( grid%landusef(i,grid%islake,j) >= 0.5 ) .OR. ( grid%lu_index(i,j) == grid%islake ) )  THEN
                     grid%sst(i,j) = grid%tavgsfc(i,j)
                     grid%tsk(i,j) = grid%tavgsfc(i,j)
                  END IF
                  IF ( ( grid%sst(i,j) .LT. 150 ) .OR. ( grid%sst(i,j) .GT. 400 ) ) THEN
                     grid%sst(i,j) = grid%t2(i,j)
                  END IF
               END DO
            END DO

         ELSE     ! We don't have tavgsfc

            CALL wrf_debug ( 0 , 'No average surface temperature for use with inland lakes')

         END IF
         DO j=jts,MIN(jde-1,jte)
            DO i=its,MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%landusef(i,grid%iswater,j) = grid%landusef(i,grid%iswater,j) + &
                                                 grid%landusef(i,grid%islake,j)
               grid%landusef(i,grid%islake,j) = 0.
            END DO
         END DO
         IF ( config_flags%surface_input_source .EQ. 3 ) THEN
            DO j=jts,MIN(jde-1,jte)
               DO i=its,MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  IF ( grid%lu_index(i,j) .EQ. grid%islake ) THEN
                     grid%lu_index(i,j) = grid%iswater
                  END IF
               END DO
            END DO
         END IF

      END IF

      !  Save the grid%tsk field for later use in the sea ice surface temperature
      !  for the Noah LSM scheme.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            grid%tsk_save(i,j) = grid%tsk(i,j)
         END DO
      END DO

      !  Protect against bad grid%tsk values over water by supplying grid%sst (if it is
      !  available, and if the grid%sst is reasonable).

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
                 ( grid%sst(i,j) .GT. 170. ) .AND. ( grid%sst(i,j) .LT. 400. ) ) THEN
               grid%tsk(i,j) = grid%sst(i,j)
            ENDIF
         END DO
      END DO

      !  Take the data from the input file and store it in the variables that
      !  use the WRF naming and ordering conventions.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            IF ( grid%snow(i,j) .GE. 10. ) then
               grid%snowc(i,j) = 1.
            ELSE
               grid%snowc(i,j) = 0.0
            END IF
         END DO
      END DO

      !  Set flag integers for presence of snowh and soilw fields

      grid%ifndsnowh = flag_snowh
      IF (num_sw_levels_input .GE. 1) THEN
         grid%ifndsoilw = 1
      ELSE
         grid%ifndsoilw = 0
      END IF

      !  Set flag integers for presence of albsi, snowsi, and icedepth fields

      IF ( config_flags%seaice_albedo_opt == 2 ) THEN
          grid%ifndalbsi = flag_albsi
      ELSE
          grid%ifndalbsi = 0
      ENDIF
          
      IF ( config_flags%seaice_snowdepth_opt == 1 ) THEN
          grid%ifndsnowsi = flag_snowsi
      ELSE
          grid%ifndsnowsi = 0
      ENDIF
          
      IF ( config_flags%seaice_thickness_opt == 1 ) THEN
          grid%ifndicedepth = flag_icedepth
      ELSE
          grid%ifndicedepth = 0
      ENDIF

      !  Only certain land surface schemes are able to work with the NLCD data.

      CALL nl_get_mminlu ( grid%id , mminlu )
      write(a_message,*) 'MMINLU = ',trim(mminlu)
      CALL wrf_debug ( 1 , a_message )
      write(a_message,*) 'sf_surface_physics = ',model_config_rec%sf_surface_physics(grid%id)
      CALL wrf_debug ( 1, a_message )

      probs_with_nlcd : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( RUCLSMSCHEME, NOAHMPSCHEME, CLMSCHEME, SSIBSCHEME )
            IF ( TRIM(mminlu) .EQ. 'NLCD40' ) THEN
               CALL wrf_message ( 'NLCD40 data may be used with SLABSCHEME, LSMSCHEME, PXLSMSCHEME' )
               CALL wrf_message ( 'Re-run geogrid and choose a different land cover source, or select a different sf_surface_physics option' )
               CALL wrf_error_fatal ( 'NLCD40 data may not be used with: RUCLSMSCHEME, NOAHMPSCHEME, CLMSCHEME, SSIBSCHEME' )
            END IF

         CASE ( SLABSCHEME, LSMSCHEME, PXLSMSCHEME )
               CALL wrf_debug ( 1, 'NLCD40 being used with an OK scheme' )

      END SELECT probs_with_nlcd

      !  We require input data for the various LSM schemes.

      enough_data : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( LSMSCHEME, NOAHMPSCHEME )
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for Noah LSM scheme.')
            END IF

         CASE (RUCLSMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for RUC LSM scheme.')
            END IF

         CASE (PXLSMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for P-X LSM scheme.')
            END IF
         CASE (CLMSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for CLM LSM scheme.')
            END IF
!---------- fds (06/2010) ---------------------------------
         CASE (SSIBSCHEME)
            IF ( num_st_levels_input .LT. 2 ) THEN
               CALL wrf_error_fatal ( 'Not enough soil temperature data for SSIB LSM scheme.')
            END IF
            IF ( eta_levels(2) .GT. 0.982 ) THEN
               CALL wrf_error_fatal ( 'The first two eta levels are too shallow for SSIB LSM scheme.')
            END IF
!--------------------------------------------------------

      END SELECT enough_data

      interpolate_soil_tmw : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME,LSMSCHEME,NOAHMPSCHEME,RUCLSMSCHEME,PXLSMSCHEME,CLMSCHEME,SSIBSCHEME )
            CALL process_soil_real ( grid%tsk , grid%tmn , grid%tavgsfc,  &
                                  grid%landmask , grid%sst , grid%ht, grid%toposoil, &
                                  st_input , sm_input , sw_input , &
                                  st_levels_input , sm_levels_input , sw_levels_input , &
                                  grid%zs , grid%dzs , model_config_rec%flag_sm_adj , &
                                  grid%tslb , grid%smois , grid%sh2o , &
                                  flag_sst , flag_tavgsfc, flag_soilhgt, &
                                  flag_soil_layers, flag_soil_levels, &
                                  ids , ide , jds , jde , kds , kde , &
                                  ims , ime , jms , jme , kms , kme , &
                                  its , ite , jts , jte , kts , kte , &
                                  model_config_rec%sf_surface_physics(grid%id) , &
                                  model_config_rec%num_soil_layers , &
                                  model_config_rec%real_data_init_type , &
                                  num_st_levels_input , num_sm_levels_input , num_sw_levels_input , &
                                  num_st_levels_alloc , num_sm_levels_alloc , num_sw_levels_alloc )

      END SELECT interpolate_soil_tmw

      !  surface_input_source=1 => use data from static file (fractional category as input)
      !  surface_input_source=2 => use data from grib file (dominant category as input)
      !  surface_input_source=3 => use dominant data from static file (dominant category as input)

      IF ( any_valid_points ) THEN
      IF ( config_flags%surface_input_source .EQ. 1 ) THEN

      !  Generate the vegetation and soil category information from the fractional input
      !  data, or use the existing dominant category fields if they exist.

         grid%vegcat (its,jts) = 0
         grid%soilcat(its,jts) = 0

         num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
         num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
         num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )

         CALL process_percent_cat_new ( grid%landmask , &
                                    grid%landusef , grid%soilctop , grid%soilcbot , &
                                    grid%isltyp , grid%ivgtyp , &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    ids , ide , jds , jde , kds , kde , &
                                    ims , ime , jms , jme , kms , kme , &
                                    its , ite , jts , jte , kts , kte , &
                                    model_config_rec%iswater(grid%id) )

         !  Make all the veg/soil parms the same so as not to confuse the developer.


         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%vegcat(i,j)  = grid%ivgtyp(i,j)
               grid%soilcat(i,j) = grid%isltyp(i,j)
            END DO
         END DO

      ELSE IF ( config_flags%surface_input_source .EQ. 2 ) THEN

         !  Do we have dominant soil and veg data from the input already?

         IF ( grid%soilcat(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%isltyp(i,j) = NINT( grid%soilcat(i,j) )
               END DO
            END DO
         END IF
         IF ( grid%vegcat(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%ivgtyp(i,j) = NINT( grid%vegcat(i,j) )
               END DO
            END DO
         END IF

      ELSE IF ( config_flags%surface_input_source .EQ. 3 ) THEN

         !  Do we have dominant soil and veg data from the static input already?

         IF ( grid%sct_dom_gc(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%isltyp(i,j) = NINT( grid%sct_dom_gc(i,j) )
                  grid%soilcat(i,j) = grid%isltyp(i,j)
               END DO
            END DO
         ELSE
            WRITE ( a_message , * ) 'You have set surface_input_source = 3,'// &
                                    ' but your geogrid data does not have valid dominant soil data.'
            CALL wrf_error_fatal ( a_message )
         END IF
         IF ( grid%lu_index(i_valid,j_valid) .GT. 0.5 ) THEN
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%ivgtyp(i,j) = NINT( grid%lu_index(i,j) )
                  grid%vegcat(i,j) = grid%ivgtyp(i,j)
               END DO
            END DO
         ELSE
            WRITE ( a_message , * ) 'You have set surface_input_source = 3,'//&
                                    ' but your geogrid data does not have valid dominant land use data.'
            CALL wrf_error_fatal ( a_message )
         END IF

         !  Need to match isltyp to landmask

         iforce = 0
         change_soil = 0
         change_soilw = 0
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF ( grid%landmask(i,j) .GT. 0.5 .AND. grid%isltyp(i,j) .EQ. grid%isoilwater ) THEN
                  grid%isltyp(i,j) = 8
                  change_soilw =  change_soilw + 1
                  iforce =  iforce + 1
               ELSE IF ( grid%landmask(i,j) .LT. 0.5 .AND. grid%isltyp(i,j) .NE. grid%isoilwater ) THEN
                  grid%isltyp(i,j) = grid%isoilwater
                  change_soil =  change_soil + 1
                  iforce =  iforce + 1
               END IF
            END DO
         END DO
         IF ( change_soilw .GT. 0 .OR. change_soil .GT. 0 ) THEN
            WRITE(a_message,FMT='(A,I4,A,I6)' ) &
                  'forcing artificial silty clay loam at ',iforce,' points, out of ',&
                  (MIN(ide-1,ite)-its+1)*(MIN(jde-1,jte)-jts+1)
                  CALL wrf_debug(0,a_message)
         END IF

      END IF

      !  Split NUDAPT  Urban Parameters

      IF ( ( config_flags%sf_urban_physics == 1 ) .OR. ( config_flags%sf_urban_physics == 2 ) .OR. ( config_flags%sf_urban_physics == 3 ) ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
              IF ( MMINLU == 'NLCD40' .OR. MMINLU == 'MODIFIED_IGBP_MODIS_NOAH') THEN
                  IF ( grid%FRC_URB2D(i,j) .GE. 0.5 .AND.  &
                      (grid%ivgtyp(i,j).NE.13 .AND. grid%ivgtyp(i,j).NE.24 .AND. grid%ivgtyp(i,j).NE.25 .AND. grid%ivgtyp(i,j).NE.26)) grid%ivgtyp(i,j)=13
              ELSE IF ( MMINLU == "USGS" ) THEN
                  IF ( grid%FRC_URB2D(i,j) .GE. 0.5 .AND.  &
                       grid%ivgtyp(i,j).NE.1 ) grid%ivgtyp(i,j)=1
              ENDIF

              IF ( grid%FRC_URB2D(i,j) == 0. ) THEN
                 IF ( (MMINLU == 'NLCD40' .OR. MMINLU == 'MODIFIED_IGBP_MODIS_NOAH') .AND. &
                   (grid%ivgtyp(i,j)==24 .OR. grid%ivgtyp(i,j)==25 .OR. grid%ivgtyp(i,j)==26 .OR. grid%ivgtyp(i,j)==13) ) grid%FRC_URB2D(i,j) = 0.9
                 IF ( MMINLU == 'USGS' .AND. grid%ivgtyp(i,j)==1 ) grid%FRC_URB2D(i,j) = 0.9
              ENDIF
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE 
               grid%LP_URB2D(i,j)  = grid%URB_PARAM(i,91,j)
               grid%LB_URB2D(i,j)  = grid%URB_PARAM(i,95,j)
               grid%HGT_URB2D(i,j)  = grid%URB_PARAM(i,94,j)
            END DO
         END DO
      ENDIF

      IF ( ( config_flags%sf_urban_physics == 2 ) .OR. ( config_flags%sf_urban_physics == 3 ) ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               DO k = 1, 15
                  grid%HI_URB2D(i,k,j)  = grid%URB_PARAM(i,k+117,j)
               END DO
            END DO
         END DO
      ENDIF

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            IF ( config_flags%sf_urban_physics==1 ) THEN
               grid%MH_URB2D(i,j)  = grid%URB_PARAM(i,92,j)
               grid%STDH_URB2D(i,j)  = grid%URB_PARAM(i,93,j)
            ENDIF
         END DO
      END DO

      DO  j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            DO k = 1, 4
               IF ( config_flags%sf_urban_physics==1 ) THEN
                  grid%LF_URB2D(i,k,j)  = grid%URB_PARAM(i,k+95,j)
               ENDIF
            END DO
         END DO
      END DO

      END IF

      !  Adjustments for the seaice field PRIOR to the grid%tslb computations.  This is
      !  is for the 5-layer scheme.


      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold )
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_pre ( grid%xice , grid%landmask , grid%tsk , grid%ivgtyp , grid%vegcat , grid%lu_index , &
                                   grid%xland , grid%landusef , grid%isltyp , grid%soilcat , grid%soilctop , &
                                   grid%soilcbot , grid%tmn , &
                                   grid%seaice_threshold , &
                                   config_flags%fractional_seaice, &
                                   num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                   grid%iswater , grid%isice , &
                                   model_config_rec%sf_surface_physics(grid%id) , &
                                   ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte )

      !  Land use assignment.

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            grid%lu_index(i,j) = grid%ivgtyp(i,j)
            IF ( grid%lu_index(i,j) .NE. model_config_rec%iswater(grid%id) ) THEN
               grid%landmask(i,j) = 1
               grid%xland(i,j)    = 1
            ELSE
               grid%landmask(i,j) = 0
               grid%xland(i,j)    = 2
            END IF
         END DO
      END DO


      !  Fix grid%tmn and grid%tsk.

      fix_tsk_tmn : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

         CASE ( SLABSCHEME , LSMSCHEME , NOAHMPSCHEME , RUCLSMSCHEME, PXLSMSCHEME,CLMSCHEME, SSIBSCHEME )
            DO j = jts, MIN(jde-1,jte)
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) .AND. &
                       ( grid%sst(i,j) .GT. 170. ) .AND. ( grid%sst(i,j) .LT. 400. ) ) THEN
                     grid%tmn(i,j) = grid%sst(i,j)
                     grid%tsk(i,j) = grid%sst(i,j)
                  ELSE IF ( grid%landmask(i,j) .LT. 0.5 ) THEN
                     grid%tmn(i,j) = grid%tsk(i,j)
                  END IF
               END DO
            END DO
      END SELECT fix_tsk_tmn

      !  Is the grid%tsk reasonable?

      IF ( internal_time_loop .NE. 1 ) THEN
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF ( grid%tsk(i,j) .LT. 170 .or. grid%tsk(i,j) .GT. 400. ) THEN
                  grid%tsk(i,j) = grid%t_2(i,1,j)
               END IF
            END DO
         END DO
      ELSE
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF ( grid%tsk(i,j) .LT. 170 .or. grid%tsk(i,j) .GT. 400. ) THEN
                  print *,'error in the grid%tsk'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
                  if(grid%tmn(i,j).gt.170. .and. grid%tmn(i,j).lt.400.)then
                     grid%tsk(i,j)=grid%tmn(i,j)
                  else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                     grid%tsk(i,j)=grid%sst(i,j)
                  else
                     CALL wrf_error_fatal ( 'grid%tsk unreasonable' )
                  end if
               END IF
            END DO
         END DO
      END IF

      !  Is the grid%tmn reasonable?

      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            IF ( ( ( grid%tmn(i,j) .LT. 170. ) .OR. ( grid%tmn(i,j) .GT. 400. ) ) &
               .AND. ( grid%landmask(i,j) .GT. 0.5 ) ) THEN
               IF ( ( model_config_rec%sf_surface_physics(grid%id) .NE. LSMSCHEME ) .and. &
                    ( model_config_rec%sf_surface_physics(grid%id) .NE. NOAHMPSCHEME ) ) THEN
                  print *,'error in the grid%tmn'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
               END IF

               if(grid%tsk(i,j).gt.170. .and. grid%tsk(i,j).lt.400.)then
                  grid%tmn(i,j)=grid%tsk(i,j)
               else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                  grid%tmn(i,j)=grid%sst(i,j)
               else
                  CALL wrf_error_fatal ( 'grid%tmn unreasonable' )
               endif
            END IF
         END DO
      END DO
   

      !  Minimum soil values, residual, from RUC LSM scheme.  For input from Noah or EC, and using
      !  RUC LSM scheme, this must be subtracted from the input total soil moisture.  For
      !  input RUC data and using the Noah LSM scheme, this value must be added to the soil
      !  moisture input.

      lqmi(1:num_soil_top_cat) = &
      (/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
        0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
        0.004, 0.065 /)
!       0.004, 0.065, 0.020, 0.004, 0.008 /)  !  has extra levels for playa, lava, and white sand

      ! If Unified Model soil moisture input, add lqmi since UM gives us available soil moisture, not total (AFWA source)
      IF ( flag_um_soil == 1 ) THEN
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               grid%smois(i,:,j)=grid%smois(i,:,j)+lqmi(grid%isltyp(i,j))
            END DO
         END DO
      END IF

      !  At the initial time we care about values of soil moisture and temperature, other times are
      !  ignored by the model, so we ignore them, too.

      IF ( domain_ClockIsStartTime(grid) ) THEN
         account_for_zero_soil_moisture : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )

            CASE ( LSMSCHEME , NOAHMPSCHEME )
               iicount = 0
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'Noah -> Noah: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1

                           grid%smois(i,:,j) = 0.005
!+---+-----------------------------------------------------------------+
!  Some bad values of soil moisture are possible (huge negative and positive), but they
!  appear to occur only along coastlines, so instead of overwriting with small moisture
!  values, use relatively large moisture val.  Orig code checked for large negative but
!  not positive values, mods here reset either.  G. Thompson (28 Feb 2008).

!                          grid%smois(i,:,j) = 0.499
!                       ELSEIF ( (grid%landmask(i,j).gt.0.5) .and. (grid%tslb(i,1,j) .gt. 170 ) .and. &
!                            ( grid%tslb(i,1,j) .lt. 400 ) .and. (grid%smois(i,1,j) .gt. 1.005 ) ) then
!                          print *,'Noah -> Noah: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
!                          iicount = iicount + 1
!                          grid%smois(i,:,j) = 0.499
!+---+-----------------------------------------------------------------+
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'Noah -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) , 0.005 )
!                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) + lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'RUC -> Noah: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.005
!+---+-----------------------------------------------------------------+
!  Same comment as above.
!                          grid%smois(i,:,j) = 0.499
!                       ELSEIF ( (grid%landmask(i,j).gt.0.5) .and. (grid%tslb(i,1,j) .gt. 170 ) .and. &
!                            ( grid%tslb(i,1,j) .lt. 400 ) .and. (grid%smois(i,1,j) .gt. 1.005 ) ) then
!                          print *,'Noah -> Noah: bad soil moisture at i,j =',i,j,grid%smois(i,:,j)
!                          iicount = iicount + 1
!                          grid%smois(i,:,j) = 0.499
!+---+-----------------------------------------------------------------+
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'RUC -> Noah: total number of small soil moisture locations = ',iicount
                  END IF
               END IF

!+---+-----------------------------------------------------------------+
               !  Fudge soil moisture higher where canopy water is non-zero.
               !  G. Thompson (12 Jun 2008)

!              DO j = jts, MIN(jte,jde-1)
!                DO i = its, MIN(ite,ide-1)
!                   IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
!                   if (grid%canwat(i,j) .GT. 1.01 .AND. grid%landmask(i,j) .GT. 0.5 ) THEN
!                      print *,'  CANWAT: moisten soil a bit more at i,j =',i,j,grid%canwat(i,j)
!                      grid%smois(i,1,j) = grid%smois(i,1,j) + (grid%canwat(i,j)**0.33333)*0.04
!                      grid%smois(i,1,j) = MIN(0.499, grid%smois(i,1,j))
!                      grid%smois(i,2,j) = grid%smois(i,2,j) + (grid%canwat(i,j)**0.33333)*0.01
!                      grid%smois(i,2,j) = MIN(0.499, grid%smois(i,2,j))
!                   end if
!                END DO
!              END DO
!+---+-----------------------------------------------------------------+


            CASE ( RUCLSMSCHEME )
               iicount = 0
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j)  , 0.005 )
!                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) - lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  ! no op
               END IF

             CASE ( PXLSMSCHEME )
               iicount = 0
               IF ( flag_soil_layers == 1 ) THEN
                  ! no op
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) , 0.005 )
!                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) + lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
               END IF
            CASE ( CLMSCHEME )
               iicount = 0
               IF      ( flag_soil_layers == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'CLM -> CLM: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'CLM -> CLM: total number of small soil moisture locations = ',iicount
                  END IF
               ELSE IF ( flag_soil_levels == 1 ) THEN
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        grid%smois(i,:,j) = MAX ( grid%smois(i,:,j) + lqmi(grid%isltyp(i,j)) , 0.005 )
                     END DO
                  END DO
                  DO j = jts, MIN(jde-1,jte)
                     DO i = its, MIN(ide-1,ite)
                        IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                        IF ( (grid%landmask(i,j).gt.0.5) .and. ( grid%tslb(i,1,j) .gt. 170 ) .and. &
                             ( grid%tslb(i,1,j) .lt. 400 ) .and. ( grid%smois(i,1,j) .lt. 0.005 ) ) then
                           print *,'CLM -> CLM: bad soil moisture at i,j = ',i,j,grid%smois(i,:,j)
                           iicount = iicount + 1
                           grid%smois(i,:,j) = 0.005
                        END IF
                     END DO
                  END DO
                  IF ( iicount .GT. 0 ) THEN
                     print *,'CLM -> CLM: total number of small soil moisture locations = ',iicount
                  END IF
               END IF

         END SELECT account_for_zero_soil_moisture
      END IF

      !  Is the grid%tslb reasonable?

      IF ( internal_time_loop .NE. 1 ) THEN
         DO j = jts, MIN(jde-1,jte)
            DO ns = 1 , model_config_rec%num_soil_layers
               DO i = its, MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  IF ( grid%tslb(i,ns,j) .LT. 170 .or. grid%tslb(i,ns,j) .GT. 400. ) THEN
                     grid%tslb(i,ns,j) = grid%t_2(i,1,j)
                     grid%smois(i,ns,j) = 0.3
                  END IF
               END DO
            END DO
         END DO
      ELSE
         DO j = jts, MIN(jde-1,jte)
            DO i = its, MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF ( ( ( grid%tslb(i,1,j) .LT. 170. ) .OR. ( grid%tslb(i,1,j) .GT. 400. ) ) .AND. &
                       ( grid%landmask(i,j) .GT. 0.5 ) ) THEN
                     IF ( ( model_config_rec%sf_surface_physics(grid%id) .NE. LSMSCHEME    ) .AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. NOAHMPSCHEME ) .AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. RUCLSMSCHEME ).AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. SSIBSCHEME ).AND. & !fds
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. CLMSCHEME ).AND. &
                          ( model_config_rec%sf_surface_physics(grid%id) .NE. PXLSMSCHEME ) ) THEN
                        print *,'error in the grid%tslb'
                        print *,'i,j=',i,j
                        print *,'grid%landmask=',grid%landmask(i,j)
                        print *,'grid%tsk, grid%sst, grid%tmn=',grid%tsk(i,j),grid%sst(i,j),grid%tmn(i,j)
                        print *,'grid%tslb = ',grid%tslb(i,:,j)
                        print *,'old grid%smois = ',grid%smois(i,:,j)
                        grid%smois(i,1,j) = 0.3
                        grid%smois(i,2,j) = 0.3
                        grid%smois(i,3,j) = 0.3
                        grid%smois(i,4,j) = 0.3
                     END IF

                     IF ( (grid%tsk(i,j).GT.170. .AND. grid%tsk(i,j).LT.400.) .AND. &
                          (grid%tmn(i,j).GT.170. .AND. grid%tmn(i,j).LT.400.) ) THEN
                        fake_soil_temp : SELECT CASE ( model_config_rec%sf_surface_physics(grid%id) )
                           CASE ( SLABSCHEME )
                              DO ns = 1 , model_config_rec%num_soil_layers
                                 grid%tslb(i,ns,j) = ( grid%tsk(i,j)*(3.0 - grid%zs(ns)) + &
                                                       grid%tmn(i,j)*(0.0 - grid%zs(ns)) ) /(3.0 - 0.0)
                              END DO
                           CASE ( LSMSCHEME , NOAHMPSCHEME , RUCLSMSCHEME, PXLSMSCHEME,CLMSCHEME,SSIBSCHEME )
!                             CALL wrf_error_fatal ( 'Assigned constant soil moisture to 0.3, stopping')
                              DO ns = 1 , model_config_rec%num_soil_layers
                                 grid%tslb(i,ns,j) = ( grid%tsk(i,j)*(3.0 - grid%zs(ns)) + &
                                                       grid%tmn(i,j)*(0.0 - grid%zs(ns)) ) /(3.0 - 0.0)
                              END DO
                        END SELECT fake_soil_temp
                     else if(grid%tsk(i,j).gt.170. .and. grid%tsk(i,j).lt.400.)then
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 1' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%tsk(i,j)
                        END DO
                     else if(grid%sst(i,j).gt.170. .and. grid%sst(i,j).lt.400.)then
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 2' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%sst(i,j)
                        END DO
                     else if(grid%tmn(i,j).gt.170. .and. grid%tmn(i,j).lt.400.)then
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 3' )
                        DO ns = 1 , model_config_rec%num_soil_layers
                           grid%tslb(i,ns,j)=grid%tmn(i,j)
                        END DO
                     else
                        CALL wrf_error_fatal ( 'grid%tslb unreasonable 4' )
                     endif
               END IF
            END DO
         END DO
      END IF

      !  Adjustments for the seaice field AFTER the grid%tslb computations.  This is
      !  is for the Noah LSM scheme.

      num_veg_cat      = SIZE ( grid%landusef , DIM=2 )
      num_soil_top_cat = SIZE ( grid%soilctop , DIM=2 )
      num_soil_bot_cat = SIZE ( grid%soilcbot , DIM=2 )
      CALL nl_get_seaice_threshold ( grid%id , grid%seaice_threshold )
      CALL nl_get_isice ( grid%id , grid%isice )
      CALL nl_get_iswater ( grid%id , grid%iswater )
      CALL adjust_for_seaice_post ( grid%xice , grid%landmask , grid%tsk , grid%tsk_save , &
                                    grid%ivgtyp , grid%vegcat , grid%lu_index , &
                                    grid%xland , grid%landusef , grid%isltyp , grid%soilcat ,  &
                                    grid%soilctop , &
                                    grid%soilcbot , grid%tmn , grid%vegfra , &
                                    grid%tslb , grid%smois , grid%sh2o , &
                                    grid%seaice_threshold , &
                                    grid%sst,flag_sst, &
                                    config_flags%fractional_seaice, &
                                    num_veg_cat , num_soil_top_cat , num_soil_bot_cat , &
                                    model_config_rec%num_soil_layers , &
                                    grid%iswater , grid%isice , &
                                    model_config_rec%sf_surface_physics(grid%id) , &
                                    ids , ide , jds , jde , kds , kde , &
                                    ims , ime , jms , jme , kms , kme , &
                                    its , ite , jts , jte , kts , kte )

      !  Let us make sure (again) that the grid%landmask and the veg/soil categories match.

oops1=0
oops2=0
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            IF ( ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. &
                   ( grid%ivgtyp(i,j) .NE. config_flags%iswater .OR. grid%isltyp(i,j) .NE. 14 ) ) .OR. &
                 ( ( grid%landmask(i,j) .GT. 0.5 ) .AND. &
                   ( grid%ivgtyp(i,j) .EQ. config_flags%iswater .OR. grid%isltyp(i,j) .EQ. 14 ) ) ) THEN
               IF ( grid%tslb(i,1,j) .GT. 1. ) THEN
oops1=oops1+1
                  grid%ivgtyp(i,j) = 5
                  grid%isltyp(i,j) = 8
                  grid%landmask(i,j) = 1
                  grid%xland(i,j) = 1
               ELSE IF ( grid%sst(i,j) .GT. 1. ) THEN
oops2=oops2+1
                  grid%ivgtyp(i,j) = config_flags%iswater
                  grid%isltyp(i,j) = 14
                  grid%landmask(i,j) = 0
                  grid%xland(i,j) = 2
               ELSE
                  print *,'the grid%landmask and soil/veg cats do not match'
                  print *,'i,j=',i,j
                  print *,'grid%landmask=',grid%landmask(i,j)
                  print *,'grid%ivgtyp=',grid%ivgtyp(i,j)
                  print *,'grid%isltyp=',grid%isltyp(i,j)
                  print *,'iswater=', config_flags%iswater
                  print *,'grid%tslb=',grid%tslb(i,:,j)
                  print *,'grid%sst=',grid%sst(i,j)
                  CALL wrf_error_fatal ( 'mismatch_landmask_ivgtyp' )
               END IF
            END IF
         END DO
      END DO
if (oops1.gt.0) then
print *,'points artificially set to land : ',oops1
endif
if(oops2.gt.0) then
print *,'points artificially set to water: ',oops2
endif
! fill grid%sst array with grid%tsk if missing in real input (needed for time-varying grid%sst in wrf)
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
           IF ( flag_sst .NE. 1 ) THEN
             grid%sst(i,j) = grid%tsk(i,j)
           ENDIF
         END DO
      END DO
!tgs set snoalb to land value if the water point is covered with ice
      DO j = jts, MIN(jde-1,jte)
         DO i = its, MIN(ide-1,ite)
           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
           IF ( grid%ivgtyp(i,j) .EQ. config_flags%isice) THEN
             grid%snoalb(i,j) = 0.75
           ENDIF
         END DO
      END DO

      !  From the full level data, we can get the half levels, reciprocals, and layer
      !  thicknesses.  These are all defined at half level locations, so one less level.
      !  We allow the vertical coordinate to *accidently* come in upside down.  We want
      !  the first full level to be the ground surface.

      !  Check whether grid%znw (full level) data are truly full levels. If not, we need to adjust them
      !  to be full levels.
      !  in this test, we check if grid%znw(1) is neither 0 nor 1 (within a tolerance of 10**-5)

      were_bad = .false.
      IF ( ( (grid%znw(1).LT.(1-1.E-5) ) .OR. ( grid%znw(1).GT.(1+1.E-5) ) ).AND. &
           ( (grid%znw(1).LT.(0-1.E-5) ) .OR. ( grid%znw(1).GT.(0+1.E-5) ) ) ) THEN
         were_bad = .true.
         print *,'Your grid%znw input values are probably half-levels. '
         print *,grid%znw
         print *,'WRF expects grid%znw values to be full levels. '
         print *,'Adjusting now to full levels...'
         !  We want to ignore the first value if it's negative
         IF (grid%znw(1).LT.0) THEN
            grid%znw(1)=0
         END IF
         DO k=2,kde
            grid%znw(k)=2*grid%znw(k)-grid%znw(k-1)
         END DO
      END IF

      !  Let's check our changes

      IF ( ( ( grid%znw(1) .LT. (1-1.E-5) ) .OR. ( grid%znw(1) .GT. (1+1.E-5) ) ).AND. &
           ( ( grid%znw(1) .LT. (0-1.E-5) ) .OR. ( grid%znw(1) .GT. (0+1.E-5) ) ) ) THEN
         print *,'The input grid%znw height values were half-levels or erroneous. '
         print *,'Attempts to treat the values as half-levels and change them '
         print *,'to valid full levels failed.'
         CALL wrf_error_fatal("bad grid%znw values from input files")
      ELSE IF ( were_bad ) THEN
         print *,'...adjusted. grid%znw array now contains full eta level values. '
      ENDIF

      IF ( grid%znw(1) .LT. grid%znw(kde) ) THEN
         DO k=1, kde/2
            hold_znw = grid%znw(k)
            grid%znw(k)=grid%znw(kde+1-k)
            grid%znw(kde+1-k)=hold_znw
         END DO
      END IF

      DO k=1, kde-1
         grid%dnw(k) = grid%znw(k+1) - grid%znw(k)
         grid%rdnw(k) = 1./grid%dnw(k)
         grid%znu(k) = 0.5*(grid%znw(k+1)+grid%znw(k))
      END DO

      !  Now the same sort of computations with the half eta levels, even ANOTHER
      !  level less than the one above.

      DO k=2, kde-1
         grid%dn(k) = 0.5*(grid%dnw(k)+grid%dnw(k-1))
         grid%rdn(k) = 1./grid%dn(k)
         grid%fnp(k) = .5* grid%dnw(k  )/grid%dn(k)
         grid%fnm(k) = .5* grid%dnw(k-1)/grid%dn(k)
      END DO

      !  Scads of vertical coefficients.

      cof1 = (2.*grid%dn(2)+grid%dn(3))/(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(2)
      cof2 =     grid%dn(2)        /(grid%dn(2)+grid%dn(3))*grid%dnw(1)/grid%dn(3)

      grid%cf1  = grid%fnp(2) + cof1
      grid%cf2  = grid%fnm(2) - cof1 - cof2
      grid%cf3  = cof2

      grid%cfn  = (.5*grid%dnw(kde-1)+grid%dn(kde-1))/grid%dn(kde-1)
      grid%cfn1 = -.5*grid%dnw(kde-1)/grid%dn(kde-1)

      !  Inverse grid distances.

      grid%rdx = 1./config_flags%dx
      grid%rdy = 1./config_flags%dy

      !  Some of the many weird geopotential initializations that we'll see today: grid%ph0 is total,
      !  and grid%ph_2 is a perturbation from the base state geopotential.  We set the base geopotential
      !  at the lowest level to terrain elevation * gravity.

      DO j=jts,jte
         DO i=its,ite
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            grid%ph0(i,1,j) = grid%ht(i,j) * g
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
    
            !  Base state pressure is a function of eta level and terrain, only, plus
            !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
            !  temperature, K), and A (temperature difference, from 1000 mb to 300 mb, K).

            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )


            DO k = 1, kte-1
               grid%php(i,k,j) = grid%c3f(k)*(p_surf - grid%p_top)+grid%c4f(k) + grid%p_top ! temporary, full lev base pressure
               grid%pb(i,k,j)  = grid%c3h(k)*(p_surf - grid%p_top)+grid%c4h(k) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
               IF ( grid%pb(i,k,j) .LT. p_strat ) THEN
                   temp = tiso + A_strat * LOG ( grid%pb(i,k,j)/p_strat )
               ENDIF
!              temp =             t00 + A*LOG(grid%pb(i,k,j)/p00)
               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO
            grid%php(i,kte,j) = grid%p_top
            !  Base state mu is defined as base state surface pressure minus grid%p_top
            grid%MUB(i,j) = p_surf - grid%p_top
            !  Dry surface pressure is defined as the following (this mu is from the input file
            !  computed from the dry pressure).  Here the dry pressure is just reconstituted.
            pd_surf = grid%MU0(i,j) + grid%p_top
            !  Integrate base geopotential, starting at terrain elevation.  This assures that
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.
            grid%phb(i,1,j) = grid%ht(i,j) * g
            IF (grid%hypsometric_opt == 1) THEN
               DO kk  = 2,kte
                  k = kk-1
                  grid%phb(i,kk,j) = grid%phb(i,kk-1,j) - grid%dnw(kk-1)*(grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))*grid%alb(i,kk-1,j)
               END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
               DO k = 2,kte
                  pfu = grid%c3f(k  )*grid%MUB(i,j)+grid%c4f(k  ) + grid%p_top
                  pfd = grid%c3f(k-1)*grid%MUB(i,j)+grid%c4f(k-1) + grid%p_top
                  phm = grid%c3h(k-1)*grid%MUB(i,j)+grid%c4h(k-1) + grid%p_top
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO
            ELSE
               CALL wrf_error_fatal( 'initialize_real: hypsometric_opt should be 1 or 2' )
            END IF

         END DO
      END DO

!+---+-----------------------------------------------------------------+
      ! New addition by Greg Thompson to dry out the stratosphere.
!     CALL wrf_debug ( 0 , ' calling routine to dry stratosphere')
!     CALL dry_stratos ( grid%t_2, moist(:,:,:,P_QV), grid%phb, &
!                        ids , ide , jds , jde , kds , kde , &
!                        ims , ime , jms , jme , kms , kme , &
!                        its , ite , jts , jte , kts , kte )
!+---+-----------------------------------------------------------------+

      !  Fill in the outer rows and columns to allow us to be sloppy.

      IF ( ite .EQ. ide ) THEN
      i = ide
      DO j = jts, MIN(jde-1,jte)
         grid%MUB(i,j) = grid%MUB(i-1,j)
         grid%MU_2(i,j) = grid%MU_2(i-1,j)
         DO k = 1, kte-1
            grid%pb(i,k,j) = grid%pb(i-1,k,j)
            grid%t_init(i,k,j) = grid%t_init(i-1,k,j)
            grid%alb(i,k,j) = grid%alb(i-1,k,j)
         END DO
         DO k = 1, kte
            grid%phb(i,k,j) = grid%phb(i-1,k,j)
         END DO
      END DO
      END IF

      IF ( jte .EQ. jde ) THEN
      j = jde
      DO i = its, ite
         grid%MUB(i,j) = grid%MUB(i,j-1)
         grid%MU_2(i,j) = grid%MU_2(i,j-1)
         DO k = 1, kte-1
            grid%pb(i,k,j) = grid%pb(i,k,j-1)
            grid%t_init(i,k,j) = grid%t_init(i,k,j-1)
            grid%alb(i,k,j) = grid%alb(i,k,j-1)
         END DO
         DO k = 1, kte
            grid%phb(i,k,j) = grid%phb(i,k,j-1)
         END DO
      END DO
      END IF

      !  Compute the total column perturbation dry pressure (grid%mub + grid%mu_2 + ptop = dry grid%psfc).

      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            grid%MU_2(i,j) = grid%MU0(i,j) - grid%MUB(i,j)
         END DO
      END DO

      !  Fill in the outer rows and columns to allow us to be sloppy.

      IF ( ite .EQ. ide ) THEN
      i = ide
      DO j = jts, MIN(jde-1,jte)
         grid%MU_2(i,j) = grid%MU_2(i-1,j)
      END DO
      END IF

      IF ( jte .EQ. jde ) THEN
      j = jde
      DO i = its, ite
         grid%MU_2(i,j) = grid%MU_2(i,j-1)
      END DO
      END IF

      lev500 = 0
      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

            !  Assign the potential temperature (perturbation from t0) and qv on all the mass
            !  point locations.

            DO k =  1 , kde-1
               grid%t_2(i,k,j)          = grid%t_2(i,k,j) - t0
            END DO

            dpmu = 10001.
            loop_count = 0

            DO WHILE ( ( ABS(dpmu) .GT. 10. ) .AND. &
                       ( loop_count .LT. 5 ) )

               loop_count = loop_count + 1

               !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
               !  equation) down from the top to get the pressure perturbation.  First get the pressure
               !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.

               kk = kte-1
               k = kk+1

               qtot=0.
               DO im = PARAM_FIRST_SCALAR, num_3d_m
                 qtot = qtot + moist(i,kk,j,im)
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2

               grid%p(i,kk,j) = - 0.5*((grid%c1f(k)*grid%Mu_2(i,j))+qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/grid%rdnw(kk)/qvf2
               qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf&
                                 *(((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)

               !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
               !  inverse density fields (total and perturbation).

               DO kk=kte-2,1,-1
                  k = kk + 1
                  qtot=0.
                  DO im = PARAM_FIRST_SCALAR, num_3d_m
                    qtot = qtot + 0.5*(moist(i,kk,j,im)+moist(i,kk+1,j,im))
                  ENDDO
                  qvf2 = 1./(1.+qtot)
                  qvf1 = qtot*qvf2
                  grid%p(i,kk,j) = grid%p(i,kk+1,j) - ((grid%c1f(k)*grid%Mu_2(i,j)) + qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/qvf2/grid%rdn(kk+1)
                  qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
                  grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                              (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
                  grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
                  grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
               END DO

#if 1
               !  This is the hydrostatic equation used in the model after the small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the geopotential.

               IF (grid%hypsometric_opt == 1) THEN
                  DO kk  = 2,kte
                     k = kk - 1
                     grid%ph_2(i,kk,j) = grid%ph_2(i,kk-1,j) - &
                                   grid%dnw(kk-1) * ( ((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*grid%al(i,kk-1,j) &
                                 + (grid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,kk-1,j) )
                     grid%ph0(i,kk,j) = grid%ph_2(i,kk,j) + grid%phb(i,kk,j)
                  END DO
               ELSE IF (grid%hypsometric_opt == 2) THEN
                ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is dry pressure.
                ! Note that al*p approximates Rd*T and dLOG(p) does z.
                ! Here T varies mostly linear with z, the first-order integration produces better result.

                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kte
                     pfu = grid%c3f(k  )*grid%MU0(i,j)+grid%c4f(k  ) + grid%p_top
                     pfd = grid%c3f(k-1)*grid%MU0(i,j)+grid%c4f(k-1) + grid%p_top
                     phm = grid%c3h(k-1)*grid%MU0(i,j)+grid%c4h(k-1) + grid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO

                  DO k = 1,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO
               END IF
#else
               !  Get the perturbation geopotential from the 3d height array from WPS.

               DO k  = 2,kte
                  grid%ph_2(i,k,j) = grid%ph0(i,k,j)*g - grid%phb(i,k,j)
               END DO
#endif

               !  Recompute density, simlar to what the model does.

               IF (grid%hypsometric_opt == 1) THEN
               DO k=kts,kte-1
                  grid%al(i,k,j)=-1./((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*(grid%alb(i,k,j)*(grid%c1h(k)*grid%mu_2(i,j))  &
                                 +grid%rdnw(k)*(grid%ph_2(i,k+1,j)-grid%ph_2(i,k,j)))
               ENDDO
               ELSE IF (grid%hypsometric_opt == 2) THEN
               DO k=kts,kte-1
                  pfu = grid%c3f(k+1)*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4f(k+1)+grid%p_top
                  pfd = grid%c3f(k  )*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4f(k  )+grid%p_top
                  phm = grid%c3h(k  )*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4h(k  )+grid%p_top
                  qvf=-1./((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*(grid%alb(i,k,j)*(grid%c1h(k)*grid%mu_2(i,j))  &
                                 +grid%rdnw(k)*(grid%ph_2(i,k+1,j)-grid%ph_2(i,k,j)))
                  grid%al(i,k,j) = (grid%ph_2(i,k+1,j)-grid%ph_2(i,k,j)+grid%phb(i,k+1,j)-grid%phb(i,k,j)) &
                                    /phm/LOG(pfd/pfu)-grid%alb(i,k,j)
#if 0
if ( internal_time_loop .EQ. 1 ) THEN
if (i.eq.its .and. j.eq.its)then
if (k.eq.kts)then
print *,'                             k        old al          new al             alb            new alt           dz (m)        pres up          Pres mid        Pres down           c3 k           c3 k+1             c4 k            c4 k+1'
print *,' ======================================================================================================================================================================================================================================='
endif
print *,'                  ',k,qvf,grid%al(i,k,j),grid%alb(i,k,j),grid%al(i,k,j)+grid%alb(i,k,j),(grid%ph_2(i,k+1,j)-grid%ph_2(i,k,j)+grid%phb(i,k+1,j)-grid%phb(i,k,j)),pfu,phm,pfd,grid%c3f(k),grid%c3f(k+1),grid%c4f(k),grid%c4f(k+1)
endif
endif
#endif
          
               ENDDO     
               END IF
     
               !  Compute pressure similarly to how computed within model.

               DO k=kts,kte-1
                  qvf = 1.+rvovrd*moist(i,k,j,P_QV)
                  grid%p(i,k,j)=p1000mb*( (r_d*(t0+grid%t_2(i,k,j))*qvf)/                     &
                               (p1000mb*(grid%al(i,k,j)+grid%alb(i,k,j))) )**cpovcv  &
                               -grid%pb(i,k,j)
                  grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
                  grid%alt(i,k,j) = grid%al(i,k,j) + grid%alb(i,k,j)
               ENDDO

               !  Adjust the column pressure so that the computed 500 mb height is close to the
               !  input value (of course, not when we are doing hybrid input).

               IF ( ( flag_metgrid .EQ. 1 ) .AND. ( i .EQ. i_valid ) .AND. ( j .EQ. j_valid ) ) THEN
                  DO k = 1 , num_metgrid_levels
                     IF ( ABS ( grid%p_gc(i,k,j) - 50000. ) .LT. 1. ) THEN
                        lev500 = k
                        EXIT
                     END IF
                  END DO
               END IF

               !  We only do the adjustment of height if we have the input data on pressure
               !  surfaces, and folks have asked to do this option.

               IF ( ( flag_metgrid .EQ. 1 ) .AND. &
                    ( flag_ptheta  .EQ. 0 ) .AND. &
                    ( config_flags%adjust_heights ) .AND. &
                    ( lev500 .NE. 0 ) ) THEN

                  DO k = 2 , kte-1

                     !  Get the pressures on the full eta levels (grid%php is defined above as
                     !  the full-lev base pressure, an easy array to use for 3d space).

                     pl = grid%php(i,k  ,j) + &
                          ( grid%p(i,k-1  ,j) * ( grid%znw(k    ) - grid%znu(k  ) ) + &
                            grid%p(i,k    ,j) * ( grid%znu(k-1  ) - grid%znw(k  ) ) ) / &
                          ( grid%znu(k-1  ) - grid%znu(k  ) )
                     pu = grid%php(i,k+1,j) + &
                          ( grid%p(i,k-1+1,j) * ( grid%znw(k  +1) - grid%znu(k+1) ) + &
                            grid%p(i,k  +1,j) * ( grid%znu(k-1+1) - grid%znw(k+1) ) ) / &
                          ( grid%znu(k-1+1) - grid%znu(k+1) )

                     !  If these pressure levels trap 500 mb, use them to interpolate
                     !  to the 500 mb level of the computed height.

                     IF ( ( pl .GE. 50000. ) .AND. ( pu .LT. 50000. ) ) THEN
                        zl = ( grid%ph_2(i,k  ,j) + grid%phb(i,k  ,j) ) / g
                        zu = ( grid%ph_2(i,k+1,j) + grid%phb(i,k+1,j) ) / g

                        z500 = ( zl * ( LOG(50000.) - LOG(pu    ) ) + &
                                 zu * ( LOG(pl    ) - LOG(50000.) ) ) / &
                               ( LOG(pl) - LOG(pu) )
!                       z500 = ( zl * (    (50000.) -    (pu    ) ) + &
!                                zu * (    (pl    ) -    (50000.) ) ) / &
!                              (    (pl) -    (pu) )

                        !  Compute the difference of the 500 mb heights (computed minus input), and
                        !  then the change in grid%mu_2.  The grid%php is still full-levels, base pressure.

                        dz500 = z500 - grid%ght_gc(i,lev500,j)
                        tvsfc = ((grid%t_2(i,1,j)+t0)*((grid%p(i,1,j)+grid%php(i,1,j))/p1000mb)**(r_d/cp)) * &
                                (1.+0.6*moist(i,1,j,P_QV))
                        dpmu = ( grid%php(i,1,j) + grid%p(i,1,j) ) * EXP ( g * dz500 / ( r_d * tvsfc ) )
                        dpmu = dpmu - ( grid%php(i,1,j) + grid%p(i,1,j) )
                        grid%MU_2(i,j) = grid%MU_2(i,j) - dpmu
                        EXIT
                     END IF

                  END DO
               ELSE
                  dpmu = 0.
               END IF

            END DO

         END DO
      END DO

      !  Now we have full pressure on eta levels, get final computation of Qv.
      !  The use of u_1 (rh) and v_1 (temperature) is temporary.

      grid%v_1 = grid%t_2+t0

      CALL theta_to_t ( grid%v_1 , grid%p_hyd  , p00 , &
                        ids , ide , jds , jde , kds , kde , &
                        ims , ime , jms , jme , kms , kme , &
                        its , ite , jts , jte , kts , kte )

      IF      ( config_flags%rh2qv_method .eq. 1 ) THEN
         CALL rh_to_mxrat1(grid%u_1, grid%v_1, grid%p_hyd , moist(:,:,:,P_QV) ,       &
                           config_flags%rh2qv_wrt_liquid ,                        &
                           config_flags%qv_max_p_safe ,                           &
                           config_flags%qv_max_flag , config_flags%qv_max_value , &
                           config_flags%qv_min_p_safe ,                           &
                           config_flags%qv_min_flag , config_flags%qv_min_value , &
                           ids , ide , jds , jde , kds , kde ,                    &
                           ims , ime , jms , jme , kms , kme ,                    &
                           its , ite , jts , jte , kts , kte-1 )
      ELSE IF ( config_flags%rh2qv_method .eq. 2 ) THEN
         CALL rh_to_mxrat2(grid%u_1, grid%v_1, grid%p_hyd , moist(:,:,:,P_QV) ,       &
                           config_flags%rh2qv_wrt_liquid ,                        &
                           config_flags%qv_max_p_safe ,                           &
                           config_flags%qv_max_flag , config_flags%qv_max_value , &
                           config_flags%qv_min_p_safe ,                           &
                           config_flags%qv_min_flag , config_flags%qv_min_value , &
                           ids , ide , jds , jde , kds , kde ,                    &
                           ims , ime , jms , jme , kms , kme ,                    &
                           its , ite , jts , jte , kts , kte-1 )
      END IF
     
      !  Compute pressure similarly to how computed within model, with final Qv.

      !  Do a re-balance or not?  0 = NOPE

      IF ( ( config_flags%rebalance .EQ. 0 ) .OR. &
           ( ( config_flags%rebalance .EQ. 2 ) .AND.  ( config_flags%vert_refine_method .NE. 2 ) ) ) THEN

          DO j = jts, min(jde-1,jte)
             DO k=kts,kte-1
                DO i = its, min(ide,ite)
                   qvf = 1.+rvovrd*moist(i,k,j,P_QV)
                   grid%p(i,k,j)=p1000mb*( (r_d*(t0+grid%t_2(i,k,j))*qvf)/                     &
                                (p1000mb*(grid%al(i,k,j)+grid%alb(i,k,j))) )**cpovcv  &
                                -grid%pb(i,k,j)
                   grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
                ENDDO
             ENDDO
          ENDDO

      ELSE ! rebalance

         lev500 = 0
         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

               dpmu = 10001.
               loop_count = 0

               DO WHILE ( ( ABS(dpmu) .GT. 10. ) .AND. &
                          ( loop_count .LT. 5 ) )

                  loop_count = loop_count + 1

                  !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
                  !  equation) down from the top to get the pressure perturbation.  First get the pressure
                  !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.

                  kk = kte-1
                  k=kk+1

                  qtot=0.
                  DO im = PARAM_FIRST_SCALAR, num_3d_m
                    qtot = qtot + moist(i,kk,j,im)
                  ENDDO
                  qvf2 = 1./(1.+qtot)
                  qvf1 = qtot*qvf2

                  grid%p(i,kk,j) = - 0.5*((grid%c1f(k)*grid%Mu_2(i,j))+qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/grid%rdnw(kk)/qvf2
                  qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
                  grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf&
                                    *(((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
                  grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
                  grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)

                  !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
                  !  inverse density fields (total and perturbation).

                  DO kk=kte-2,1,-1
                     k = kk + 1
                     qtot=0.
                     DO im = PARAM_FIRST_SCALAR, num_3d_m
                       qtot = qtot + 0.5*(moist(i,kk,j,im)+moist(i,kk+1,j,im))
                     ENDDO
                     qvf2 = 1./(1.+qtot)
                     qvf1 = qtot*qvf2
                     grid%p(i,kk,j) = grid%p(i,kk+1,j) - ((grid%c1f(k)*grid%Mu_2(i,j)) + qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/qvf2/grid%rdn(kk+1)
                     qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
                     grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                                 (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
                     grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
                     grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
                  END DO

#if 1
                  !  This is the hydrostatic equation used in the model after the small timesteps.  In
                  !  the model, grid%al (inverse density) is computed from the geopotential.

                  IF (grid%hypsometric_opt == 1) THEN
        
                     DO kk  = 2,kte
                        k = kk-1
                        grid%ph_2(i,kk,j) = grid%ph_2(i,kk-1,j) - &
                                      grid%dnw(kk-1) * ( ((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*grid%al(i,kk-1,j) &
                                    + (grid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,kk-1,j) )
                        grid%ph0(i,kk,j) = grid%ph_2(i,kk,j) + grid%phb(i,kk,j)
                     END DO

                  ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is dry pressure.
                  ! Note that al*p approximates Rd*T and dLOG(p) does z.
                  ! Here T varies mostly linear with z, the first-order integration produces better result.

                  ELSE IF (grid%hypsometric_opt == 2) THEN

                     grid%ph_2(i,1,j) = grid%phb(i,1,j)
                     DO k = 2,kte
                        pfu = grid%c3f(k  )*grid%MU0(i,j)+grid%c4f(k  ) + grid%p_top
                        pfd = grid%c3f(k-1)*grid%MU0(i,j)+grid%c4f(k-1) + grid%p_top
                        phm = grid%c3h(k-1)*grid%MU0(i,j)+grid%c4h(k-1) + grid%p_top
                        grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
                     END DO

                     DO k = 1,kte
                        grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                     END DO
                  END IF
#else
                  !  Get the perturbation geopotential from the 3d height array from WPS.

                  DO k  = 2,kte
                     grid%ph_2(i,k,j) = grid%ph0(i,k,j)*g - grid%phb(i,k,j)
                  END DO
#endif

                  !  Recompute density, simlar to what the model does.

                  IF (grid%hypsometric_opt == 1) THEN
                     DO k=kts,kte-1
                        grid%al(i,k,j)=-1./((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*(grid%alb(i,k,j)*(grid%c1h(k)*grid%mu_2(i,j))  &
                                       +grid%rdnw(k)*(grid%ph_2(i,k+1,j)-grid%ph_2(i,k,j)))
                     ENDDO
                  ELSE IF (grid%hypsometric_opt == 2) THEN
                     DO k=kts,kte-1
                        pfu = grid%c3f(k+1)*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4f(k+1)+grid%p_top
                        pfd = grid%c3f(k  )*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4f(k  )+grid%p_top
                        phm = grid%c3h(k  )*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4h(k  )+grid%p_top
                        grid%al(i,k,j) = (grid%ph_2(i,k+1,j)-grid%ph_2(i,k,j)+grid%phb(i,k+1,j)-grid%phb(i,k,j)) &
                                          /phm/LOG(pfd/pfu)-grid%alb(i,k,j)
                     ENDDO     
                  END IF
        
                  !  Compute pressure similarly to how computed within model.

                  DO k=kts,kte-1
                     qvf = 1.+rvovrd*moist(i,k,j,P_QV)
                     grid%p(i,k,j)=p1000mb*( (r_d*(t0+grid%t_2(i,k,j))*qvf)/                     &
                                  (p1000mb*(grid%al(i,k,j)+grid%alb(i,k,j))) )**cpovcv  &
                                  -grid%pb(i,k,j)
                     grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
                     grid%alt(i,k,j) = grid%al(i,k,j) + grid%alb(i,k,j)
                  ENDDO

                  !  Adjust the column pressure so that the computed 500 mb height is close to the
                  !  input value (of course, not when we are doing hybrid input).

                  IF ( ( flag_metgrid .EQ. 1 ) .AND. ( i .EQ. i_valid ) .AND. ( j .EQ. j_valid ) ) THEN
                     DO k = 1 , num_metgrid_levels
                        IF ( ABS ( grid%p_gc(i,k,j) - 50000. ) .LT. 1. ) THEN
                           lev500 = k
                           EXIT
                        END IF
                     END DO
                  END IF

                  !  We only do the adjustment of height if we have the input data on pressure
                  !  surfaces, and folks have asked to do this option.

                  IF ( ( flag_metgrid .EQ. 1 ) .AND. &
                       ( flag_ptheta  .EQ. 0 ) .AND. &
                       ( config_flags%adjust_heights ) .AND. &
                       ( lev500 .NE. 0 ) ) THEN

                     DO k = 2 , kte-1

                        !  Get the pressures on the full eta levels (grid%php is defined above as
                        !  the full-lev base pressure, an easy array to use for 3d space).

                        pl = grid%php(i,k  ,j) + &
                             ( grid%p(i,k-1  ,j) * ( grid%znw(k    ) - grid%znu(k  ) ) + &
                               grid%p(i,k    ,j) * ( grid%znu(k-1  ) - grid%znw(k  ) ) ) / &
                             ( grid%znu(k-1  ) - grid%znu(k  ) )
                        pu = grid%php(i,k+1,j) + &
                             ( grid%p(i,k-1+1,j) * ( grid%znw(k  +1) - grid%znu(k+1) ) + &
                               grid%p(i,k  +1,j) * ( grid%znu(k-1+1) - grid%znw(k+1) ) ) / &
                             ( grid%znu(k-1+1) - grid%znu(k+1) )

                        !  If these pressure levels trap 500 mb, use them to interpolate
                        !  to the 500 mb level of the computed height.

                        IF ( ( pl .GE. 50000. ) .AND. ( pu .LT. 50000. ) ) THEN
                           zl = ( grid%ph_2(i,k  ,j) + grid%phb(i,k  ,j) ) / g
                           zu = ( grid%ph_2(i,k+1,j) + grid%phb(i,k+1,j) ) / g

                           z500 = ( zl * ( LOG(50000.) - LOG(pu    ) ) + &
                                    zu * ( LOG(pl    ) - LOG(50000.) ) ) / &
                                  ( LOG(pl) - LOG(pu) )

                           !  Compute the difference of the 500 mb heights (computed minus input), and
                           !  then the change in grid%mu_2.  The grid%php is still full-levels, base pressure.

                           dz500 = z500 - grid%ght_gc(i,lev500,j)
                           tvsfc = ((grid%t_2(i,1,j)+t0)*((grid%p(i,1,j)+grid%php(i,1,j))/p1000mb)**(r_d/cp)) * &
                                   (1.+0.6*moist(i,1,j,P_QV))
                           dpmu = ( grid%php(i,1,j) + grid%p(i,1,j) ) * EXP ( g * dz500 / ( r_d * tvsfc ) )
                           dpmu = dpmu - ( grid%php(i,1,j) + grid%p(i,1,j) )
                           grid%MU_2(i,j) = grid%MU_2(i,j) - dpmu
                           EXIT
                        END IF

                     END DO
                  ELSE
                     dpmu = 0.
                  END IF

               END DO

            ENDDO
         ENDDO
      END IF ! rebalance

      !  If this is data from the SI, then we probably do not have the original
      !  surface data laying around.  Note that these are all the lowest levels
      !  of the respective 3d arrays.  For surface pressure, we assume that the
      !  vertical gradient of grid%p prime is zilch.  This is not all that important.
      !  These are filled in so that the various plotting routines have something
      !  to play with at the initial time for the model.

      IF ( flag_metgrid .NE. 1 ) THEN
         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%u10(i,j)=grid%u_2(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%v10(i,j)=grid%v_2(i,1,j)
            END DO
         END DO

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
               grid%psfc(i,j)=p_surf + grid%p(i,1,j)
               grid%q2(i,j)=moist(i,1,j,P_QV)
               grid%th2(i,j)=grid%t_2(i,1,j)+300.
               grid%t2(i,j)=grid%th2(i,j)*(((grid%p(i,1,j)+grid%pb(i,1,j))/p00)**(r_d/cp))
            END DO
         END DO

      !  If this data is from WPS, then we have previously assigned the surface
      !  data for u, v, and t.  If we have an input qv, welp, we assigned that one,
      !  too.  Now we pick up the left overs, and if RH came in - we assign the
      !  mixing ratio.

      ELSE IF ( flag_metgrid .EQ. 1 ) THEN

         DO j = jts, min(jde-1,jte)
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
!              p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
!              grid%psfc(i,j)=p_surf + grid%p(i,1,j)
               grid%th2(i,j)=grid%t2(i,j)*(p00/(grid%p(i,1,j)+grid%pb(i,1,j)))**(r_d/cp)
            END DO
         END DO
         IF ( flag_qv .NE. 1 ) THEN
            DO j = jts, min(jde-1,jte)
               DO i = its, min(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
!                 grid%q2(i,j)=moist(i,1,j,P_QV)
                  grid%q2(i,j)=grid%qv_gc(i,1,j)
               END DO
            END DO
         END IF

      END IF
      CALL cpu_time(t_end)

!  Set flag to denote that we are saving original values of HT, MUB, and
!  PHB for 2-way nesting and cycling.

      grid%save_topo_from_real=1

      !  Template for initializing tracer arrays.
      !  Right now, a small plane in the middle of the domain at lowest model level is
      !  defined.

      IF (config_flags%tracer_opt .eq. 2) THEN
          DO j = (jde + jds)/2 - 4, (jde + jds)/2 + 4, 1
            DO i = (ide + ids)/2 - 4, (ide + ids)/2 + 4, 1
               IF ( its .LE. i .and. ite .GE. i .and. jts .LE. j .and. jte .GE. j ) THEN
                 tracer(i, 1, j, P_tr17_1) = 1.
                 tracer(i, 1, j, P_tr17_2) = 1.
                 tracer(i, 1, j, P_tr17_3) = 1.
                 tracer(i, 1, j, P_tr17_4) = 1.
!                tracer(i, 1, j, P_tr17_5) = 1.
!                tracer(i, 1, j, P_tr17_6) = 1.
!                tracer(i, 1, j, P_tr17_7) = 1.
!                tracer(i, 1, j, P_tr17_8) = 1.
               END IF
            END DO
          END DO
      END IF

      !  Template for initializing trajectories.  The i, j, and k starting locations
      !  are specified.  Right now, a small plane in the middle of the domain is
      !  selected.
      
      grid%traj_i    = -9999
      grid%traj_j    = -9999
      grid%traj_k    = -9999
      grid%traj_lat  = -9999
      grid%traj_long = -9999
    
      IF (config_flags%num_traj .gt. 0 .and. config_flags%traj_opt .gt. 0) THEN
         icount = 1
         DO j = (jde + jds)/2 - 2, (jde + jds)/2 + 2, 1
            DO i = (ide + ids)/2 - 2, (ide + ids)/2 + 2, 1
               IF ( its .LE. i    .and. ite .GE. i   .and.  jts .LE. j    .and. jte .GE. j ) THEN
                  grid%traj_i   (icount) = i
                  grid%traj_j   (icount) = j
                  grid%traj_k   (icount) = 10
                  grid%traj_lat (icount) = grid%xlat(i,j)
                  grid%traj_long(icount) = grid%xlong(i,j)
               END IF
         
#ifdef DM_PARALLEL
               grid%traj_i   (icount) = wrf_dm_max_real ( grid%traj_i   (icount) )
               grid%traj_j   (icount) = wrf_dm_max_real ( grid%traj_j   (icount) )
               grid%traj_k   (icount) = wrf_dm_max_real ( grid%traj_k   (icount) )
               grid%traj_lat (icount) = wrf_dm_max_real ( grid%traj_lat (icount) )
               grid%traj_long(icount) = wrf_dm_max_real ( grid%traj_long(icount) )
#endif
                
               icount = icount + 1
               IF (icount .GT. config_flags%num_traj) THEN
                  EXIT
               END IF
            END DO
         END DO  
      END IF

      !  Simple initialization for 3d ocean.  

      IF ( config_flags%sf_ocean_physics .EQ. PWP3DSCHEME ) THEN

         !  From a profile of user defined temps, depths, and salinity - we
         !  construct a 3d ocean.  Because this is a 1d profile, domains that
         !  have varied ocean characteristics that deviate should significantly from
         !  the provided initial state will probably give poor results.

         DO k = 1,model_config_rec%ocean_levels
            grid%om_depth(:,k,:) = model_config_rec%ocean_z(k)
            grid%om_tmp  (:,k,:) = model_config_rec%ocean_t(k)
            grid%om_s    (:,k,:) = model_config_rec%ocean_s(k)
            grid%om_tini (:,k,:) = model_config_rec%ocean_t(k)
            grid%om_sini (:,k,:) = model_config_rec%ocean_s(k)
            grid%om_u    (:,k,:) = 0.
            grid%om_v    (:,k,:) = 0.
         END DO

         !  Apparently, the mixed layer is 5 m.

         grid%om_ml = 5    

         !  Keep lat, lon info for the ocean model.

         grid%om_lon = grid%xlong
         grid%om_lat = grid%xlat

         !  If we have access to a non-horizontally isotropic SST, let's 
         !  use that as a better starting point for the ocean temp.  Note that
         !  we assume if this is an ice point that implies this is a land point
         !  for WRF.  If it is a land point, then we do not have any ocean underneath.

         IF ( ( grid%landmask(i,j) .LT. 0.5 ) .AND. ( flag_sst .EQ. 1 ) ) THEN
            DO j = jts, min(jde-1,jte)
               DO k = 1,model_config_rec%ocean_levels
                  DO i = its, min(ide-1,ite)
                     grid%om_tmp(i,k,j) = grid%sst(i,j) - ( grid%om_tini(i,1,j) - grid%om_tini(i,k,j) )
                  END DO
               END DO
            END DO
   
            DO j = jts, min(jde-1,jte)
               DO k = 1,model_config_rec%ocean_levels
                  DO i = its, min(ide-1,ite)
                     grid%om_tini(i,k,j) = grid%om_tmp(i,k,j)
                  END DO
               END DO
            END DO
         END IF

      END IF

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte

!+---+-----------------------------------------------------------------+
!..Scale the lowest level aerosol data into an emissions rate.  This is
!.. very far from ideal, but need higher emissions where larger amount
!.. of (climo) existing and lesser emissions where there exists fewer to 
!.. begin as a first-order simplistic approach.  Later, proper connection to
!.. emission inventory would be better, but, for now, scale like this:
!.. where: Nwfa=50 per cc, emit 0.875E4 aerosols per second per grid box unit
!..        that was tested as ~(20kmx20kmx50m = 2.E10 m**-3)
!+---+-----------------------------------------------------------------+

      IF ( config_flags%use_aero_icbc .AND. ( flag_qnwfa .EQ. 1 ) .AND. &
                  ( config_flags%mp_physics .EQ. THOMPSONAERO ) ) THEN
         do j = jts, min(jde-1,jte)
         do i = its, min(ide-1,ite)
            z1 = (grid%phb(i,2,j)-grid%phb(i,1,j))/g
            airmass = 1./grid%alt(i,1,j) * z1 * config_flags%dx*config_flags%dy          ! kg
            grid%qnwfa2d(i,j) = grid%QNWFA_now(i,1,j) * 0.000196 * (airmass*2.E-10)
     if(i.eq.its .and. j.eq.jts) then
      write(a_message,*) 'aero_sfc_flux: ', i,j,z1,1./grid%alt(i,1,j),airmass,grid%QNWFA_now(i,1,j),grid%qnwfa2d(i,j)
      CALL wrf_debug (1, a_message)
     endif
         enddo
         enddo
      ENDIF

!+---+-----------------------------------------------------------------+
!..Let us ensure that double-moment microphysics variables have numbers
!.. where there is mass.  Currently doing this for Thompson-MP only, but
!.. can consider doing it for every MP scheme that has 2-moment variables.
!.. This is important because pressure-level RAP/HRRR files have mass but
!.. not number values for example (whereas native model level files have
!.. both).
!+---+-----------------------------------------------------------------+

      IF ( config_flags%mp_physics .EQ. THOMPSON .OR.                   &
                   config_flags%mp_physics .EQ. THOMPSONAERO ) THEN

         !..As it occurs up above, temporarily utilizing the v_1 variable,
         !.. to hold temperature, which it does when time_loop=0.

         IF ( internal_time_loop .GT. 1 ) THEN
            grid%v_1 = grid%t_2+t0

            CALL theta_to_t ( grid%v_1 , grid%p_hyd  , p00 , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )
         ENDIF


         do j = jts, MIN(jte,jde-1)
         do i = its, MIN(ite,ide-1)

            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

            do k = kts, kte-1
               temp_rho = 1./grid%alt(i,k,j)

               !..Produce a sensible cloud droplet number concentration

               if (P_QNC.gt.1 .AND. moist(i,k,j,P_QC).gt.0.0 .AND. scalar(i,k,j,P_QNC).le.0.0) then
                  if (P_QNWFA .gt. 1) then
                     scalar(i,k,j,P_QNC) = make_DropletNumber (moist(i,k,j,P_QC)*temp_rho,       &
     &                           scalar(i,k,j,P_QNWFA)*temp_rho, grid%xland(i,j))
                  else
                     scalar(i,k,j,P_QNC) = make_DropletNumber (moist(i,k,j,P_QC)*temp_rho,       &
     &                           0.0, grid%xland(i,j))
                  endif
                  scalar(i,k,j,P_QNC) = scalar(i,k,j,P_QNC) / temp_rho
               endif

               !..Produce a sensible cloud ice number concentration

               if (P_QNI.gt.1 .AND. moist(i,k,j,P_QI).gt.0.0 .AND. scalar(i,k,j,P_QNI).le.0.0) then
                  scalar(i,k,j,P_QNI) = make_IceNumber (moist(i,k,j,P_QI)*temp_rho, grid%v_1(i,k,j))
                  scalar(i,k,j,P_QNI) = scalar(i,k,j,P_QNI) / temp_rho
               endif

               !..Produce a sensible rain number concentration

               if (P_QNR.gt.1 .AND. moist(i,k,j,P_QR).gt.0.0 .AND. scalar(i,k,j,P_QNR).le.0.0) then
                  scalar(i,k,j,P_QNR) = make_RainNumber (moist(i,k,j,P_QR)*temp_rho, grid%v_1(i,k,j))
                  scalar(i,k,j,P_QNR) = scalar(i,k,j,P_QNR) / temp_rho
               endif

            enddo

         enddo
         enddo

      ENDIF

!+---+-----------------------------------------------------------------+
      !  Added by Greg Thompson.  Pre-set snow depth by latitude, elevation, and day-of-year.

!     CALL wrf_debug ( 0 , ' calling routine to add snow in high mountain peaks')
!     DO j = jts, min(jde-1,jte)
!        DO i = its, min(ide-1,ite)
!           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
!           grid%snowh(i,j) = snowHires (grid%snowh(i,j), grid%xlat(i,j), grid%ht(i,j), current_date, i,j)
!           grid%snow(i,j) = grid%snowh(i,j) * 1000. / 5.
!        END DO
!     END DO
!     CALL wrf_debug ( 0 , '   DONE routine to add snow in high mountain peaks')
!+---+-----------------------------------------------------------------+

! checking whether var_sso exists in the domain
      ! if so, we set got_var_sso flag to true.  This is later used in external/RSL_LITE/module_dm.F
      ! to check for this, when the topo_wind option is used.
      grid%got_var_sso = .FALSE.
      DO j=jts,MIN(jde-1,jte)
         DO i=its,MIN(ide-1,ite)
           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF(grid%var_sso(i,j) .NE. 0) THEN
                    grid%got_var_sso = .true.
               ENDIF
         END DO
      END DO
#if ( defined(DM_PARALLEL)  &&   ! defined(STUBMPI) )
      grid%got_var_sso = wrf_dm_lor_logical ( grid%got_var_sso )
#endif

      !  Save the dry perturbation potential temperature.

      DO j = jts, min(jde-1,jte)
         DO k = kts, kte
            DO i = its, min(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               grid%th_phy_m_t0(i,k,j) = grid%t_2(i,k,j)
            END DO
         END DO
      END DO

      !  Turn dry potential temperature into moist potential temperature
      !  at the very end of this routine, just before the halo communications.
      !  This field will be in the model IC and and used to construct the 
      !  BC file.

      IF ( ( config_flags%use_theta_m .EQ. 1 ) .AND. (P_Qv .GE. PARAM_FIRST_SCALAR) ) THEN
         DO j = jts, min(jde-1,jte)
            DO k = kts, kte
               DO i = its, min(ide,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  grid%t_2(i,k,j) = ( grid%t_2(i,k,j) + T0 ) * (1. + (R_v/R_d) * moist(i,k,j,P_QV)) - T0
               END DO
            END DO
         END DO
      END IF

#ifdef DM_PARALLEL
# include "HALO_EM_INIT_1.inc"
# include "HALO_EM_INIT_2.inc"
# include "HALO_EM_INIT_3.inc"
# include "HALO_EM_INIT_4.inc"
# include "HALO_EM_INIT_5.inc"
      IF ( config_flags%sf_ocean_physics .EQ. PWP3DSCHEME ) THEN
# include "HALO_EM_INIT_6.inc"
      END IF
#endif

      RETURN

   END SUBROUTINE init_domain_rk

!---------------------------------------------------------------------

   SUBROUTINE const_module_initialize ( p00 , t00 , a , tiso , p_strat , a_strat )
      USE module_configure
      IMPLICIT NONE
      !  For the real-data-cases only.
      REAL , INTENT(OUT) :: p00 , t00 , a , tiso , p_strat , a_strat
      CALL nl_get_base_pres        ( 1 , p00     )
      CALL nl_get_base_temp        ( 1 , t00     )
      CALL nl_get_base_lapse       ( 1 , a       )
      CALL nl_get_iso_temp         ( 1 , tiso    )
      CALL nl_get_base_pres_strat  ( 1 , p_strat )
      CALL nl_get_base_lapse_strat ( 1 , a_strat )
   END SUBROUTINE const_module_initialize

!-------------------------------------------------------------------

   SUBROUTINE rebalance_driver ( grid )

      IMPLICIT NONE

      TYPE (domain)          :: grid

      CALL rebalance( grid &
!
#include "actual_new_args.inc"
!
      )

   END SUBROUTINE rebalance_driver

!---------------------------------------------------------------------

   SUBROUTINE rebalance ( grid  &
!
#include "dummy_new_args.inc"
!
                        )
      IMPLICIT NONE

      TYPE (domain)          :: grid

#include "dummy_new_decl.inc"

      TYPE (grid_config_rec_type)              :: config_flags

      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2
      REAL :: p00 , t00 , a , tiso , p_strat , a_strat
      REAL , DIMENSION(:,:,:) , ALLOCATABLE :: t_init_int

      !  Local domain indices and counters.

      INTEGER :: num_veg_cat , num_soil_top_cat , num_soil_bot_cat

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k, kk

      REAL    :: temp, temp_int
      REAL    :: pfu, pfd, phm
      REAL    :: w1, w2, z0, z1, z2

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

      END SELECT

      ALLOCATE ( t_init_int(ims:ime,kms:kme,jms:jme) )

      !  Fill config_flags the options for a particular domain

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      !  Some of the many weird geopotential initializations that we'll see today: grid%ph0 is total,
      !  and grid%ph_2 is a perturbation from the base state geopotential.  We set the base geopotential
      !  at the lowest level to terrain elevation * gravity.

      DO j=jts,jte
         DO i=its,ite
            grid%ph0(i,1,j) = grid%ht_fine(i,j) * g
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      !  To define the base state, we call a USER MODIFIED routine to set the three
      !  necessary constants:  p00 (sea level pressure, Pa), t00 (sea level temperature, K),
      !  and A (temperature difference, from 1000 mb to 300 mb, K), and constant stratosphere
      !  temp (tiso, K) either from input file or from namelist (for backward compatibiliy).

      IF ( config_flags%use_baseparam_fr_nml ) then
      ! get these from namelist
         CALL wrf_message('ndown: using namelist constants')
         CALL const_module_initialize ( p00 , t00 , a , tiso , p_strat , a_strat )
      ELSE
      ! get these constants from model data
         CALL wrf_debug(99,'ndown: using base-state profile constants from input file')
         t00     = grid%t00
         p00     = grid%p00
         a       = grid%tlp
         tiso    = grid%tiso
         p_strat = grid%p_strat
         a_strat = grid%tlp_strat

         IF (t00 .LT. 100. .or. p00 .LT. 10000.) THEN
            WRITE(wrf_err_message,*)&
      'ndown_em: did not find base state parameters in wrfout. Add use_baseparam_fr_nml = .t. in &dynamics and rerun'
            CALL wrf_error_fatal(TRIM(wrf_err_message))
         ENDIF
      ENDIF

      hold_ups = .true.

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            !  Base state pressure is a function of eta level and terrain, only, plus
            !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
            !  temperature, K), and A (temperature difference, from 1000 mb to 300 mb, K).
            !  The fine grid terrain is ht_fine, the interpolated is grid%ht.

            p_surf     = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht_fine(i,j)/a/r_d ) **0.5 )
            p_surf_int = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)     /a/r_d ) **0.5 )

            DO k = 1, kte-1
               grid%pb(i,k,j) = grid%c3h(k)*(p_surf     - grid%p_top) + grid%c4h(k) + grid%p_top
               pb_int    = grid%c3h(k)*(p_surf_int - grid%p_top) + grid%c4h(k) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
               IF ( grid%pb(i,k,j) .LT. p_strat ) THEN
                  temp = tiso + A_strat * LOG ( grid%pb(i,k,j)/p_strat )
               ENDIF
!              temp =             t00 + A*LOG(pb/p00)
               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
!              grid%t_init(i,k,j)    = (t00 + A*LOG(grid%pb(i,k,j)/p00))*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
               temp_int = MAX ( tiso, t00 + A*LOG(pb_int   /p00) )
               IF ( pb_int .LT. p_strat ) THEN
                  temp_int = tiso + A_strat * LOG ( pb_int/p_strat )
               ENDIF
               t_init_int(i,k,j)= temp_int*(p00/pb_int   )**(r_d/cp) - t0
!              t_init_int(i,k,j)= (t00 + A*LOG(pb_int   /p00))*(p00/pb_int   )**(r_d/cp) - t0
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO
            !  Base state mu is defined as base state surface pressure minus grid%p_top
            grid%MUB(i,j) = p_surf - grid%p_top
            !  Dry surface pressure is defined as the following (this mu is from the input file
            !  computed from the dry pressure).  Here the dry pressure is just reconstituted.
            pd_surf = ( grid%MUB(i,j) + grid%MU_2(i,j) ) + grid%p_top
            !  Integrate base geopotential, starting at terrain elevation.  This assures that
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.
            grid%phb(i,1,j) = grid%ht_fine(i,j) * g
            IF (grid%hypsometric_opt == 1) THEN
              DO kk = 2,kte
                 k = kk - 1
                 grid%phb(i,kk,j) = grid%phb(i,kk-1,j) - grid%dnw(kk-1)*(grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))*grid%alb(i,kk-1,j)
              END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
              DO k = 2,kte
                 pfu = grid%c3f(k  )*grid%MUB(i,j)+grid%c4f(k  ) + grid%p_top
                 pfd = grid%c3f(k-1)*grid%MUB(i,j)+grid%c4f(k-1) + grid%p_top
                 phm = grid%c3h(k-1)*grid%MUB(i,j)+grid%c4h(k-1) + grid%p_top
                 grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
              END DO
            ELSE
              CALL wrf_error_fatal( 'initialize_real: hypsometric_opt should be 1 or 2' )
            END IF
         END DO
      END DO
      !  Replace interpolated terrain with fine grid values.
      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            grid%ht(i,j) = grid%ht_fine(i,j)
         END DO
      END DO
      !  Perturbation fields.
      DO j = jts, min(jde-1,jte)
         DO i = its, min(ide-1,ite)
            !  The potential temperature is THETAnest = THETAinterp + ( TBARnest - TBARinterp)
            DO k =  1 , kde-1
               grid%t_2(i,k,j) = grid%t_2(i,k,j) + ( grid%t_init(i,k,j) - t_init_int(i,k,j) )
            END DO
            !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
            !  equation) down from the top to get the pressure perturbation.  First get the pressure
            !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.
            kk = kte-1
            k = kk+1
            qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk,j,P_QV))
            qvf2 = 1./(1.+qvf1)
            qvf1 = qvf1*qvf2
            grid%p(i,kk,j) = - 0.5*((grid%c1f(k)*grid%Mu_2(i,j))+qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/grid%rdnw(kk)/qvf2
            qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
            IF ( config_flags%use_theta_m .EQ. 1 ) THEN
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)* &
                                  (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
            ELSE 
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                                  (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
            END IF
            grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
            grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
            !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
            !  inverse density fields (total and perturbation).
            DO kk=kte-2,1,-1
               k = kk+1
               qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk+1,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               grid%p(i,kk,j) = grid%p(i,kk+1,j) - ((grid%c1f(k)*grid%Mu_2(i,j)) + qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/qvf2/grid%rdn(kk+1)
               qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
               IF ( config_flags%use_theta_m .EQ. 1 ) THEN
                  grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)* &
                                     (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               ELSE
                  grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                                     (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               END IF
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
            END DO
            !  This is the hydrostatic equation used in the model after the small timesteps.  In
            !  the model, grid%al (inverse density) is computed from the geopotential.
            IF (grid%hypsometric_opt == 1) THEN
               DO kk  = 2,kte
                  k = kk-1
                  grid%ph_2(i,kk,j) = grid%ph_2(i,kk-1,j) - &
                                grid%dnw(kk-1) * ( ((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*grid%al(i,kk-1,j) &
                              + (grid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,kk-1,j) )
                  grid%ph0(i,kk,j) = grid%ph_2(i,kk,j) + grid%phb(i,kk,j)
               END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
             ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is dry pressure.
             ! Note that al*p approximates Rd*T and dLOG(p) does z.
             ! Here T varies mostly linear with z, the first-order integration produces better result.
               grid%ph_2(i,1,j) = grid%phb(i,1,j)
               DO k = 2,kte
                  pfu = grid%c3f(k  )*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4f(k  )+grid%p_top
                  pfd = grid%c3f(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4f(k-1)+grid%p_top
                  phm = grid%c3h(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j))+grid%c4h(k-1)+grid%p_top
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO

               DO k = 1,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
               END DO

               DO k = 1,kte
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO

            END IF

! update psfc in fine grid

            z0 = grid%ph0(i,1,j)/g
            z1 = 0.5*(grid%ph0(i,1,j)+grid%ph0(i,2,j))/g
            z2 = 0.5*(grid%ph0(i,2,j)+grid%ph0(i,3,j))/g
            w1 = (z0 - z2)/(z1 - z2)
            w2 = 1. - w1
            grid%psfc(i,j) = w1*(grid%p(i,1,j)+grid%pb(i,1,j))+w2*(grid%p(i,2,j)+grid%pb(i,2,j))

         END DO
      END DO

      DEALLOCATE ( t_init_int )

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte
#ifdef DM_PARALLEL
# include "HALO_EM_INIT_1.inc"
# include "HALO_EM_INIT_2.inc"
# include "HALO_EM_INIT_3.inc"
# include "HALO_EM_INIT_4.inc"
# include "HALO_EM_INIT_5.inc"
#endif
   END SUBROUTINE rebalance

!---------------------------------------------------------------------

   RECURSIVE SUBROUTINE find_my_parent ( grid_ptr_in , grid_ptr_out , id_i_am , id_wanted , found_the_id )

!  RAR - Modified to correct problem in which the parent of a child domain could
!  not be found in the namelist. This condition typically occurs while using the
!  "allow_grid" namelist option when an inactive domain comes before an active
!  domain in the list, i.e., the domain number of the active domain is greater than
!  that of an inactive domain at the same level.
!      
      USE module_domain

      TYPE(domain) , POINTER :: grid_ptr_in , grid_ptr_out
      TYPE(domain) , POINTER :: grid_ptr_sibling
      INTEGER :: id_wanted , id_i_am
      INTEGER :: nest                    ! RAR
      LOGICAL :: found_the_id

      found_the_id = .FALSE.
      grid_ptr_sibling => grid_ptr_in
      nest = 0                           ! RAR

      DO WHILE ( ASSOCIATED ( grid_ptr_sibling ) )

         IF ( grid_ptr_sibling%grid_id .EQ. id_wanted ) THEN
            found_the_id = .TRUE.
            grid_ptr_out => grid_ptr_sibling
            RETURN
! RAR    ELSE IF ( grid_ptr_sibling%num_nests .GT. 0 ) THEN
         ELSE IF ( grid_ptr_sibling%num_nests .GT. 0 .AND. nest .LT. grid_ptr_sibling%num_nests ) THEN
            nest = nest + 1               ! RAR
            grid_ptr_sibling => grid_ptr_sibling%nests(nest)%ptr ! RAR
            CALL find_my_parent ( grid_ptr_sibling , grid_ptr_out , id_i_am , id_wanted , found_the_id )
            IF (.NOT. found_the_id) grid_ptr_sibling => grid_ptr_sibling%parents(1)%ptr   ! RAR
         ELSE
            grid_ptr_sibling => grid_ptr_sibling%sibling
         END IF

      END DO

   END SUBROUTINE find_my_parent

!---------------------------------------------------------------------

   RECURSIVE SUBROUTINE find_my_parent2 ( grid_ptr_in , grid_ptr_out , id_wanted , found_the_id )

      USE module_domain

      TYPE(domain) , POINTER               :: grid_ptr_in
      TYPE(domain) , POINTER               :: grid_ptr_out
      INTEGER                , INTENT(IN ) :: id_wanted
      LOGICAL                , INTENT(OUT) :: found_the_id

      !  Local

      TYPE(domain) , POINTER :: grid_ptr_holder
      INTEGER :: kid

      !  Initializations

      found_the_id = .FALSE.
      grid_ptr_holder => grid_ptr_in


      !  Have we found the correct location?  If so, we can just pop back up with
      !  the pointer to the right location (i.e. the parent), thank you very much.

      IF ( id_wanted .EQ. grid_ptr_in%grid_id ) THEN

         found_the_id = .TRUE.
         grid_ptr_out => grid_ptr_in


      !  We gotta keep looking.

      ELSE

      !  We drill down and process each nest from this domain.  We don't have to 
      !  worry about siblings, as we are running over all of the kids for this parent,
      !  so it amounts to the same set of domains being tested.  

         loop_over_all_kids : DO kid = 1 , grid_ptr_in%num_nests

            IF ( ASSOCIATED ( grid_ptr_in%nests(kid)%ptr ) ) THEN

               CALL find_my_parent2 ( grid_ptr_in%nests(kid)%ptr , grid_ptr_out , id_wanted , found_the_id )
               IF ( found_the_id ) THEN
                  EXIT loop_over_all_kids
               END IF

            END IF
         END DO loop_over_all_kids

      END IF

   END SUBROUTINE find_my_parent2

#endif

!---------------------------------------------------------------------

#ifdef VERT_UNIT

!gfortran -DVERT_UNIT -ffree-form -ffree-line-length-none module_initialize_real.F -o vert.exe

!This is a main program for a small unit test for the vertical interpolation.

program vint

   implicit none

   integer , parameter :: ij = 3
   integer , parameter :: keta = 30
   integer , parameter :: kgen =20

   integer :: ids , ide , jds , jde , kds , kde , &
              ims , ime , jms , jme , kms , kme , &
              its , ite , jts , jte , kts , kte

   integer :: generic

   real , dimension(1:ij,kgen,1:ij) :: fo , po
   real , dimension(1:ij,1:keta,1:ij) :: fn_calc , fn_interp , pn
   real , dimension(1:ij,1:ij) :: not_required_2d_1, not_required_2d_2, &
                                  not_required_2d_3, not_required_2d_4, &
                                  not_required_2d_5, not_required_2d_6

   integer, parameter :: interp_type             = 1 ! 2
   integer, parameter :: extrap_type             = 2 ! 1
!  integer, parameter :: lagrange_order          = 2 ! 1
   integer            :: lagrange_order
   logical, parameter :: lowest_lev_from_sfc     = .FALSE. ! .TRUE.
   logical, parameter :: use_levels_below_ground = .TRUE. ! .FALSE. ! .TRUE.
   logical, parameter :: use_surface             = .TRUE. ! .FALSE. ! .TRUE.
   real   , parameter :: zap_close_levels        = 500. ! 100.
   integer, parameter :: force_sfc_in_vinterp    = 6 ! 0 ! 6
   integer, parameter :: id                      = 1

   integer :: k

   ids = 1 ; ide = ij ; jds = 1 ; jde = ij ; kds = 1 ; kde = keta
   ims = 1 ; ime = ij ; jms = 1 ; jme = ij ; kms = 1 ; kme = keta
   its = 1 ; ite = ij ; jts = 1 ; jte = ij ; kts = 1 ; kte = keta

   generic = kgen

   print *,' '
   print *,'------------------------------------'
   print *,'UNIT TEST FOR VERTICAL INTERPOLATION'
   print *,'------------------------------------'
   print *,' '
   do lagrange_order = 1 , 1
      print *,' '
      print *,'------------------------------------'
      print *,'Lagrange Order = ',lagrange_order
      print *,'------------------------------------'
      print *,' '
      call fillitup ( fo , po , fn_calc , pn , &
                    ids , ide , jds , jde , kds , kde , &
                    ims , ime , jms , jme , kms , kme , &
                    its , ite , jts , jte , kts , kte , &
                    generic , lagrange_order )

      print *,' '
      print *,'Level   Pressure     Field'
      print *,'          (Pa)      (generic)'
      print *,'------------------------------------'
      print *,' '
      do k = 1 , generic
      write (*,fmt='(i2,2x,f12.3,1x,g15.8)' ) &
         k,po(2,k,2),fo(2,k,2)
      end do
      print *,' '

      call vert_interp ( fo , po , fn_interp , pn , &
                         not_required_2d_1, not_required_2d_2, &
                         not_required_2d_3, not_required_2d_4, &
                         not_required_2d_5, not_required_2d_6, &
                         0 , 0, 5000., 5000., 30000., &
                         generic , 'T' , &
                         interp_type , lagrange_order , extrap_type , &
                         lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                         zap_close_levels , force_sfc_in_vinterp , id , &
                         ids , ide , jds , jde , kds , kde , &
                         ims , ime , jms , jme , kms , kme , &
                         its , ite , jts , jte , kts , kte )

      print *,'Multi-Order Interpolator'
      print *,'------------------------------------'
      print *,' '
      print *,'Level  Pressure      Field           Field         Field'
      print *,'         (Pa)        Calc            Interp        Diff'
      print *,'------------------------------------'
      print *,' '
      do k = kts , kte-1
      write (*,fmt='(i2,2x,f12.3,1x,3(g15.7))' ) &
         k,pn(2,k,2),fn_calc(2,k,2),fn_interp(2,k,2),fn_calc(2,k,2)-fn_interp(2,k,2)
      end do

   end do

end program vint

subroutine wrf_error_fatal (string)
   character (len=*) :: string
   print *,string
   stop
end subroutine wrf_error_fatal

subroutine fillitup ( fo , po , fn , pn , &
                    ids , ide , jds , jde , kds , kde , &
                    ims , ime , jms , jme , kms , kme , &
                    its , ite , jts , jte , kts , kte , &
                    generic , lagrange_order )

   implicit none

   integer , intent(in) :: ids , ide , jds , jde , kds , kde , &
              ims , ime , jms , jme , kms , kme , &
              its , ite , jts , jte , kts , kte

   integer , intent(in) :: generic , lagrange_order

   real , dimension(ims:ime,generic,jms:jme) , intent(out) :: fo , po
   real , dimension(ims:ime,kms:kme,jms:jme) , intent(out) :: fn , pn

   integer :: i , j , k

   k = 1
   do j = jts , jte
   do i = its , ite
      po(i,k,j) = 102000.
   end do
   end do

   do k = 2 , generic
   do j = jts , jte
   do i = its , ite
      po(i,k,j) = ( 5000. * ( 1 - (k-1) ) + 100000. * ( (k-1) - (generic-1) ) ) / (1. - real(generic-1) )
!     po(i,k,j) = FILL IN YOUR INPUT PRESSURE LEVELS
   end do
   end do
   end do

   if ( lagrange_order .eq. 1 ) then
      do k = 1 , generic
      do j = jts , jte
      do i = its , ite
         fo(i,k,j) = po(i,k,j)
!        fo(i,k,j) = FILL IN YOUR COLUMN OF PRESS_LEVEL FIELD
      end do
      end do
      end do
   else if ( lagrange_order .eq. 2 ) then
      do k = 1 , generic
      do j = jts , jte
      do i = its , ite
         fo(i,k,j) = (((po(i,k,j)-5000.)/102000.)*((102000.-po(i,k,j))/102000.))*102000.
      end do
      end do
      end do
   end if

!!!!!!!!!!!!

   do k = kts , kte
   do j = jts , jte
   do i = its , ite
      pn(i,k,j) = ( 5000. * ( 0 - (k-1) ) + 102000. * ( (k-1) - (kte-1) ) ) / (-1. *  real(kte-1) )
!     pn(i,k,j) = FILL IN A COLUMN OF KNOWN FULL-LEVEL PRESSURES ON ETA SURFACES
   end do
   end do
   end do

   do k = kts , kte-1
   do j = jts , jte
   do i = its , ite
      pn(i,k,j) = ( pn(i,k,j) + pn(i,k+1,j) ) /2.
   end do
   end do
   end do


   if ( lagrange_order .eq. 1 ) then
      do k = kts , kte-1
      do j = jts , jte
      do i = its , ite
         fn(i,k,j) = pn(i,k,j)
!        fn(i,k,j) = FILL IN COLUMN OF HALF LEVEL FIELD
      end do
      end do
      end do
   else if ( lagrange_order .eq. 2 ) then
      do k = kts , kte-1
      do j = jts , jte
      do i = its , ite
         fn(i,k,j) = (((pn(i,k,j)-5000.)/102000.)*((102000.-pn(i,k,j))/102000.))*102000.
!        fn(i,k,j) = sin(pn(i,k,j) * piov2 / 102000. )
      end do
      end do
      end do
   end if

end subroutine fillitup

function skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups )
   logical :: skip_middle_points_t
   integer :: ids , ide , jds , jde , i , j , em_width
   logical :: hold_ups
   skip_middle_points_t = .false.
end function skip_middle_points_t

subroutine wrf_message(level,message)
   character(len=*), intent(in) :: message
   integer, intent(in) :: level
   print *,trim(message)
end subroutine wrf_message

#endif

!---------------------------------------------------------------------

   SUBROUTINE vert_interp ( fo , po , fnew , pnu , &
                            fo_maxw , fo_trop , po_maxw , po_trop , &
                            po_maxwnn , po_tropnn , &
                            flag_maxw , flag_trop , &
                            maxw_horiz_pres_diff , trop_horiz_pres_diff , &
                            maxw_above_this_level , &
                            generic , var_type , &
                            interp_type , lagrange_order , extrap_type , &
                            lowest_lev_from_sfc , use_levels_below_ground , use_surface , &
                            zap_close_levels , force_sfc_in_vinterp , id , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   !  Vertically interpolate the new field.  The original field on the original
   !  pressure levels is provided, and the new pressure surfaces to interpolate to.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: interp_type , lagrange_order , extrap_type
      LOGICAL , INTENT(IN)        :: lowest_lev_from_sfc , use_levels_below_ground , use_surface
      REAL    , INTENT(IN)        :: zap_close_levels
      REAL    , INTENT(IN)        :: maxw_horiz_pres_diff , trop_horiz_pres_diff , maxw_above_this_level
      INTEGER , INTENT(IN)        :: force_sfc_in_vinterp , id
      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      INTEGER , INTENT(IN)        :: generic
      INTEGER , INTENT(IN)        :: flag_maxw , flag_trop

      CHARACTER (LEN=1) :: var_type

      REAL , DIMENSION(ims:ime,generic,jms:jme) , INTENT(IN)     :: fo , po
      REAL , DIMENSION(ims:ime,jms:jme)         , INTENT(IN)     :: fo_maxw , fo_trop , po_maxw , po_trop
      REAL , DIMENSION(ims:ime,jms:jme)         , INTENT(IN)     :: po_maxwnn , po_tropnn
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: pnu
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: fnew

      REAL , DIMENSION(ims:ime,generic,jms:jme)                  :: forig , porig
      REAL , DIMENSION(ims:ime,jms:jme)                          :: forig_maxw , forig_trop , porig_maxw , porig_trop
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)                  :: pnew

      !  Local vars

      CHARACTER (LEN=256) :: message
      INTEGER :: i , j , k , ko , kn , k1 , k2 , ko_1 , ko_2 , knext
      INTEGER :: istart , iend , jstart , jend , kstart , kend
      INTEGER , DIMENSION(ims:ime,kms:kme        )               :: k_above , k_below
      INTEGER , DIMENSION(ims:ime                )               :: ks
      INTEGER , DIMENSION(ims:ime                )               :: ko_above_sfc
      INTEGER :: count , zap , zap_below , zap_above , kst , kcount
      INTEGER :: kinterp_start , kinterp_end , sfc_level

      LOGICAL :: any_below_ground

      REAL :: p1 , p2 , pn, hold , zap_close_extra_levels
      REAL , DIMENSION(1:generic+flag_maxw+flag_trop) :: ordered_porig , ordered_forig
      REAL , DIMENSION(kts:kte) :: ordered_pnew , ordered_fnew
    
      !  Excluded middle.

      LOGICAL :: any_valid_points
      INTEGER :: i_valid , j_valid
      LOGICAL :: flip_data_required
#ifdef VERT_UNIT
      LOGICAL, EXTERNAL :: skip_middle_points_t
      INTEGER :: em_width
      LOGICAL :: hold_ups
#endif
      INTEGER :: final_zap_check_count , count_close_by_at_ko

      !  Vertical interpolation of the extra levels from metgrid: max wind and tropopause

      LOGICAL :: ok_data
      INTEGER :: ii, jj

      zap_close_extra_levels = 500

      !  Horiontal loop bounds for different variable types.

      IF      ( var_type .EQ. 'U' ) THEN
         istart = its
         iend   = ite
         jstart = MAX(jds  ,jts-1)
         jend   = MIN(jde-1,jte+1)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = MAX(ids+1,its-1) , MIN(ide-1,ite+1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig(i,k,j) = ( po(i,k,j) + po(i-1,k,j) ) * 0.5
               END DO
            END DO
            DO i = MAX(ids+1,its-1) , MIN(ide-1,ite+1)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig_maxw(i,j) = ( po_maxw(i,j) + po_maxw(i-1,j) ) * 0.5
                  porig_trop(i,j) = ( po_trop(i,j) + po_trop(i-1,j) ) * 0.5
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = 1,generic
                  porig(its,k,j) =  po(its,k,j)
               END DO
               porig_maxw(its,j) =  po_maxw(its,j)
               porig_trop(its,j) =  po_trop(its,j)
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = 1,generic
                  porig(ite,k,j) =  po(ite-1,k,j)
               END DO
               porig_maxw(ite,j) =  po_maxw(ite-1,j)
               porig_trop(ite,j) =  po_trop(ite-1,j)
            END IF

            DO k = kstart,kend
               DO i = MAX(ids+1,its-1) , MIN(ide-1,ite+1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i-1,k,j) ) * 0.5
               END DO
            END DO
            IF ( ids .EQ. its ) THEN
               DO k = kstart,kend
                  pnew(its,k,j) =  pnu(its,k,j)
               END DO
            END IF
            IF ( ide .EQ. ite ) THEN
               DO k = kstart,kend
                  pnew(ite,k,j) =  pnu(ite-1,k,j)
               END DO
            END IF
         END DO
      ELSE IF ( var_type .EQ. 'V' ) THEN
         istart = MAX(ids  ,its-1)
         iend   = MIN(ide-1,ite+1)
         jstart = jts
         jend   = jte
         kstart = kts
         kend   = kte-1
         DO i = istart,iend
            DO k = 1,generic
               DO j = MAX(jds+1,jts-1) , MIN(jde-1,jte+1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig(i,k,j) = ( po(i,k,j) + po(i,k,j-1) ) * 0.5
               END DO
            END DO
            DO j = MAX(jds+1,jts-1) , MIN(jde-1,jte+1)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig_maxw(i,j) = ( po_maxw(i,j) + po_maxw(i,j-1) ) * 0.5
                  porig_trop(i,j) = ( po_trop(i,j) + po_trop(i,j-1) ) * 0.5
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = 1,generic
                  porig(i,k,jts) =  po(i,k,jts)
               END DO
               porig_maxw(i,jts) =  po_maxw(i,jts)
               porig_trop(i,jts) =  po_trop(i,jts)
            END IF
            IF ( jde .EQ. jte ) THEN
               DO k = 1,generic
                  porig(i,k,jte) =  po(i,k,jte-1)
               END DO
               porig_maxw(i,jte) =  po_maxw(i,jte-1)
               porig_trop(i,jte) =  po_trop(i,jte-1)
            END IF

            DO k = kstart,kend
               DO j = MAX(jds+1,jts-1) , MIN(jde-1,jte+1)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  pnew(i,k,j) = ( pnu(i,k,j) + pnu(i,k,j-1) ) * 0.5
               END DO
            END DO
            IF ( jds .EQ. jts ) THEN
               DO k = kstart,kend
                  pnew(i,k,jts) =  pnu(i,k,jts)
               END DO
            END IF
            IF ( jde .EQ. jte ) THEN
              DO k = kstart,kend
                  pnew(i,k,jte) =  pnu(i,k,jte-1)
               END DO
            END IF
         END DO
      ELSE IF ( ( var_type .EQ. 'W' ) .OR.  ( var_type .EQ. 'Z' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO
            DO i = istart,iend
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig_maxw(i,j) = po_maxw(i,j)
                  porig_trop(i,j) = po_trop(i,j)
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE IF ( ( var_type .EQ. 'T' ) .OR. ( var_type .EQ. 'Q' ) ) THEN
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO
            DO i = istart,iend
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig_maxw(i,j) = po_maxw(i,j)
                  porig_trop(i,j) = po_trop(i,j)
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      ELSE
         istart = its
         iend   = MIN(ide-1,ite)
         jstart = jts
         jend   = MIN(jde-1,jte)
         kstart = kts
         kend   = kte-1
         DO j = jstart,jend
            DO k = 1,generic
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  porig(i,k,j) = po(i,k,j)
               END DO
            END DO

            DO k = kstart,kend
               DO i = istart,iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  pnew(i,k,j) = pnu(i,k,j)
               END DO
            END DO
         END DO
      END IF

      !  We need to find if there are any valid non-excluded-middle points in this
      !  tile.  If so, then we need to hang on to a valid i,j location.

      any_valid_points = .false.
      find_valid : DO j = jstart , jend
         DO i = istart , iend
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            any_valid_points = .true.
            i_valid = i
            j_valid = j
            EXIT find_valid
         END DO
      END DO find_valid
      IF ( .NOT. any_valid_points ) THEN
         RETURN
      END IF

      IF ( porig(i_valid,2,j_valid) .LT. porig(i_valid,generic,j_valid) ) THEN
         flip_data_required = .true.
      ELSE
         flip_data_required = .false.
      END IF

      DO j = jstart , jend

         !  The lowest level is the surface.  Levels 2 through "generic" are supposed to
         !  be "bottom-up".  Flip if they are not.  This is based on the input pressure
         !  array.

         IF ( flip_data_required ) THEN
            DO kn = 2 , ( generic + 1 ) / 2
               DO i = istart , iend
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  hold                    = porig(i,kn,j)
                  porig(i,kn,j)           = porig(i,generic+2-kn,j)
                  porig(i,generic+2-kn,j) = hold
                  forig(i,kn,j)           = fo   (i,generic+2-kn,j)
                  forig(i,generic+2-kn,j) = fo   (i,kn,j)
               END DO
            END DO
            DO i = istart , iend
               forig(i,1,j)               = fo   (i,1,j)
            END DO
            IF ( MOD(generic,2) .EQ. 0 ) THEN
               k=generic/2 + 1
               DO i = istart , iend
                  forig(i,k,j)            = fo   (i,k,j)
               END DO
            END IF
         ELSE
            DO kn = 1 , generic
               DO i = istart , iend
                  forig(i,kn,j)           = fo   (i,kn,j)
               END DO
            END DO
         END IF

         !  Skip all of the levels below ground in the original data based upon the surface pressure.
         !  The ko_above_sfc is the index in the pressure array that is above the surface.  If there
         !  are no levels underground, this is index = 2.  The remaining levels are eligible for use
         !  in the vertical interpolation.

         DO i = istart , iend
            ko_above_sfc(i) = -1
         END DO
         DO ko = kstart+1 , generic
            DO i = istart , iend
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF ( ko_above_sfc(i) .EQ. -1 ) THEN
                  IF ( porig(i,1,j) .GT. porig(i,ko,j) ) THEN
                     ko_above_sfc(i) = ko
                  END IF
               END IF
            END DO
         END DO

         !  Piece together columns of the original input data.  Pass the vertical columns to
         !  the iterpolator.

         DO i = istart , iend
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

            !  If the surface value is in the middle of the array, three steps: 1) do the
            !  values below the ground (this is just to catch the occasional value that is
            !  inconsistently below the surface based on input data), 2) do the surface level, then
            !  3) add in the levels that are above the surface.  For the levels next to the surface,
            !  we check to remove any levels that are "too close".  When building the column of input
            !  pressures, we also attend to the request for forcing the surface analysis to be used
            !  in a few lower eta-levels.

            !  Fill in the column from up to the level just below the surface with the input
            !  presssure and the input field (orig or old, which ever).  For an isobaric input
            !  file, this data is isobaric.

            !  How many levels have we skipped in the input column.

            zap = 0
            zap_below = 0
            zap_above = 0

            IF (  ko_above_sfc(i) .GT. 2 ) THEN
               count = 1
               DO ko = 2 , ko_above_sfc(i)-1
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

               !  Make sure the pressure just below the surface is not "too close", this
               !  will cause havoc with the higher order interpolators.  In case of a "too close"
               !  instance, we toss out the offending level (NOT the surface one) by simply
               !  decrementing the accumulating loop counter.

               IF ( ordered_porig(count-1) - porig(i,1,j) .LT. zap_close_levels ) THEN
                  count = count -1
                  zap = 1
                  zap_below = 1
               END IF

               !  Add in the surface values.

               ordered_porig(count) = porig(i,1,j)
               ordered_forig(count) = forig(i,1,j)
               count = count + 1

               !  A usual way to do the vertical interpolation is to pay more attention to the
               !  surface data.  Why?  Well it has about 20x the density as the upper air, so we
               !  hope the analysis is better there.  We more strongly use this data by artificially
               !  tossing out levels above the surface that are beneath a certain number of prescribed
               !  eta levels at this (i,j).  The "zap" value is how many levels of input we are
               !  removing, which is used to tell the interpolator how many valid values are in
               !  the column.  The "count" value is the increment to the index of levels, and is
               !  only used for assignments.

               IF ( force_sfc_in_vinterp .GT. 0 ) THEN

                  !  Get the pressure at the eta level.  We want to remove all input pressure levels
                  !  between the level above the surface to the pressure at this eta surface.  That
                  !  forces the surface value to be used through the selected eta level.  Keep track
                  !  of two things: the level to use above the eta levels, and how many levels we are
                  !  skipping.

                  knext = ko_above_sfc(i)
                  find_level : DO ko = ko_above_sfc(i) , generic
                     IF ( porig(i,ko,j) .LE. pnew(i,force_sfc_in_vinterp,j) ) THEN
                        knext = ko
                        exit find_level
                     ELSE
                        zap = zap + 1
                        zap_above = zap_above + 1
                     END IF
                  END DO find_level

               !  No request for special interpolation, so we just assign the next level to use
               !  above the surface as, ta da, the first level above the surface.  I know, wow.

               ELSE
                  knext = ko_above_sfc(i)
               END IF

               !  One more time, make sure the pressure just above the surface is not "too close", this
               !  will cause havoc with the higher order interpolators.  In case of a "too close"
               !  instance, we toss out the offending level above the surface (NOT the surface one) by simply
               !  incrementing the loop counter.  Here, count-1 is the surface level and knext is either
               !  the next level up OR it is the level above the prescribed number of eta surfaces.

               IF ( ordered_porig(count-1) - porig(i,knext,j) .LT. zap_close_levels ) THEN
                  kst = knext+1
                  zap = zap + 1
                  zap_above = zap_above + 1
               ELSE
                  kst = knext
               END IF

               DO ko = kst , generic
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            !  This is easy, the surface is the lowest level, just stick them in, in this order.  OK,
            !  there are a couple of subtleties.  We have to check for that special interpolation that
            !  skips some input levels so that the surface is used for the lowest few eta levels.  Also,
            !  we must make sure that we still do not have levels that are "too close" together.

            ELSE

               !  Initialize no input levels have yet been removed from consideration.

               zap = 0

               !  The surface is the lowest level, so it gets set right away to location 1.

               ordered_porig(1) = porig(i,1,j)
               ordered_forig(1) = forig(i,1,j)

               !  We start filling in the array at loc 2, as in just above the level we just stored.

               count = 2

               !  Are we forcing the interpolator to skip valid input levels so that the
               !  surface data is used through more levels?  Essentially as above.

               IF ( force_sfc_in_vinterp .GT. 0 ) THEN
                  knext = 2
                  find_level2: DO ko = 2 , generic
                     IF ( porig(i,ko,j) .LE. pnew(i,force_sfc_in_vinterp,j) ) THEN
                        knext = ko
                        exit find_level2
                     ELSE
                        zap = zap + 1
                        zap_above = zap_above + 1
                     END IF
                  END DO find_level2
               ELSE
                  knext = 2
               END IF

               !  Fill in the data above the surface.  The "knext" index is either the one
               !  just above the surface OR it is the index associated with the level that
               !  is just above the pressure at this (i,j) of the top eta level that is to
               !  be directly impacted with the surface level in interpolation.

               DO ko = knext , generic
                  IF ( ( ordered_porig(count-1) - porig(i,ko,j) .LT. zap_close_levels ) .AND. &
                       ( ko .LT. generic ) ) THEN
                     zap = zap + 1
                     zap_above = zap_above + 1
                     CYCLE
                  END IF
                  ordered_porig(count) = porig(i,ko,j)
                  ordered_forig(count) = forig(i,ko,j)
                  count = count + 1
               END DO

            END IF

            !  Now get the column of the "new" pressure data.  So, this one is easy.

            DO kn = kstart , kend
               ordered_pnew(kn) = pnew(i,kn,j)
            END DO

            !  How many levels (count) are we shipping to the Lagrange interpolator.

            IF      ( ( use_levels_below_ground ) .AND. ( use_surface ) ) THEN

               !  Use all levels, including the input surface, and including the pressure
               !  levels below ground.  We know to stop when we have reached the top of
               !  the input pressure data.

               count = 0
               find_how_many_1 : DO ko = 1 , generic
                  IF ( porig(i,generic,j) .EQ. ordered_porig(ko) ) THEN
                     count = count + 1
                     EXIT find_how_many_1
                  ELSE
                     count = count + 1
                  END IF
               END DO find_how_many_1
               kinterp_start = 1
               kinterp_end = kinterp_start + count - 1

            ELSE IF ( ( use_levels_below_ground ) .AND. ( .NOT. use_surface ) ) THEN

               !  Use all levels (excluding the input surface) and including the pressure
               !  levels below ground.  We know to stop when we have reached the top of
               !  the input pressure data.

               count = 0
               find_sfc_2 : DO ko = 1 , generic
                  IF ( porig(i,1,j) .EQ. ordered_porig(ko) ) THEN
                     sfc_level = ko
                     EXIT find_sfc_2
                  END IF
               END DO find_sfc_2

               DO ko = sfc_level , generic-1
                  ordered_porig(ko) = ordered_porig(ko+1)
                  ordered_forig(ko) = ordered_forig(ko+1)
               END DO
               ordered_porig(generic) = 1.E-5
               ordered_forig(generic) = 1.E10

               count = 0
               find_how_many_2 : DO ko = 1 , generic
                  IF ( porig(i,generic,j) .EQ. ordered_porig(ko) ) THEN
                     count = count + 1
                     EXIT find_how_many_2
                  ELSE
                     count = count + 1
                  END IF
               END DO find_how_many_2
               kinterp_start = 1
               kinterp_end = kinterp_start + count - 1

            ELSE IF ( ( .NOT. use_levels_below_ground ) .AND. ( use_surface ) ) THEN

               !  Use all levels above the input surface pressure.

               kcount = ko_above_sfc(i)-1-zap_below
               count = 0
               DO ko = 1 , generic
                  IF ( porig(i,ko,j) .EQ. ordered_porig(kcount) ) THEN
!  write (6,fmt='(f11.3,f11.3,g11.5)') porig(i,ko,j),ordered_porig(kcount),ordered_forig(kcount)
                     kcount = kcount + 1
                     count = count + 1
                  ELSE
!  write (6,fmt='(f11.3            )') porig(i,ko,j)
                  END IF
               END DO
               kinterp_start = ko_above_sfc(i)-1-zap_below
               kinterp_end = kinterp_start + count - 1

            END IF

            !  If we have additional levels (for example, some arrays have a "level of max winds"
            !  or a "level of the tropopause"), we insert them here.

            IF ( ( flag_maxw .EQ. 1 ) .AND. ( porig_maxw(i,j) .LE. maxw_above_this_level ) ) then
               ok_data = .TRUE.
               DO jj = -2, 2
               DO ii = -2, 2
                  ok_data = ok_data .AND. &
                  ( ABS(po_maxwnn(MAX(MIN(i+ii,iend+2,ide-1),istart-2,ids),MAX(MIN(j+jj,jend+2,jde-1),jstart-2,jds))-porig_maxw(i,j)) &
                  .LT. maxw_horiz_pres_diff )
               END DO
               END DO
               IF ( ok_data) THEN
                  insert_maxw : DO ko = kinterp_start , kinterp_end-1
                     IF ( ( ( ordered_porig(ko)-porig_maxw(i,j) ) * ( ordered_porig(ko+1)-porig_maxw(i,j) ) ) .LT. 0 ) THEN
                        IF ( ( ABS(ordered_porig(ko  )-porig_maxw(i,j)) .GT. zap_close_extra_levels ) .AND. &
                             ( ABS(ordered_porig(ko+1)-porig_maxw(i,j)) .GT. zap_close_extra_levels ) ) THEN
                           DO kcount = kinterp_end , ko+1 , -1
                              ordered_porig(kcount+1) = ordered_porig(kcount)
                              ordered_forig(kcount+1) = ordered_forig(kcount)
                           END DO
                           ordered_porig(ko+1) = porig_maxw(i,j)
                           ordered_forig(ko+1) = fo_maxw(i,j)
                           kinterp_end = kinterp_end + 1
                           EXIT insert_maxw
                        ELSE IF ( ABS(ordered_porig(ko  )-porig_maxw(i,j)) .LE. zap_close_extra_levels ) THEN
                           ordered_porig(ko) = porig_maxw(i,j)
                           ordered_forig(ko) = fo_maxw(i,j)
                           EXIT insert_maxw
                        ELSE IF ( ABS(ordered_porig(ko+1)-porig_maxw(i,j)) .LE. zap_close_extra_levels ) THEN
                           ordered_porig(ko+1) = porig_maxw(i,j)
                           ordered_forig(ko+1) = fo_maxw(i,j)
                           EXIT insert_maxw
                        END IF
                     END IF
                  END DO insert_maxw
               END IF
            END IF

            IF ( flag_trop .EQ. 1 ) THEN
               ok_data = .TRUE.
               DO jj = -2, 2
               DO ii = -2, 2
                  ok_data = ok_data .AND. &
                  ( ABS(po_tropnn(MAX(MIN(i+ii,iend+2,ide-1),istart-2,ids),MAX(MIN(j+jj,jend+2,jde-1),jstart-2,jds))-porig_trop(i,j)) &
                  .LT. trop_horiz_pres_diff )
               END DO
               END DO
               IF ( ok_data) THEN
                  insert_trop : DO ko = kinterp_start , kinterp_end-1
                     IF ( ( ( ordered_porig(ko)-porig_trop(i,j) ) * ( ordered_porig(ko+1)-porig_trop(i,j) ) ) .LT. 0 ) THEN
                        IF ( ( ABS(ordered_porig(ko  )-porig_trop(i,j)) .GT. zap_close_extra_levels ) .AND. &
                             ( ABS(ordered_porig(ko+1)-porig_trop(i,j)) .GT. zap_close_extra_levels ) ) THEN
                           DO kcount = kinterp_end , ko+1 , -1
                              ordered_porig(kcount+1) = ordered_porig(kcount)
                              ordered_forig(kcount+1) = ordered_forig(kcount)
                           END DO
                           ordered_porig(ko+1) = porig_trop(i,j)
                           ordered_forig(ko+1) = fo_trop(i,j)
                           kinterp_end = kinterp_end + 1
                           EXIT insert_trop
                        ELSE IF ( ABS(ordered_porig(ko  )-porig_trop(i,j)) .LE. zap_close_extra_levels ) THEN
                           ordered_porig(ko) = porig_trop(i,j)
                           ordered_forig(ko) = fo_trop(i,j)
                           EXIT insert_trop
                        ELSE IF ( ABS(ordered_porig(ko+1)-porig_trop(i,j)) .LE. zap_close_extra_levels ) THEN
                           ordered_porig(ko+1) = porig_trop(i,j)
                           ordered_forig(ko+1) = fo_trop(i,j)
                           EXIT insert_trop
                        END IF
                     END IF
                  END DO insert_trop
               END IF
            END IF

#if 0
            !  One final check to make sure that the delta pressures are OK.

            final_zap_check_count = 0
            DO ko = kinterp_start , kinterp_end-1

               count_close_by_at_ko = 0
               close_by_at_ko : DO

                  !  First, is the pressure difference between two neighboring layers too small?
   
                  IF ( ABS(ordered_porig(ko) - ordered_porig(ko+1)) .LT. zap_close_levels ) THEN
   
                     !  Make sure we are vertically located where this difference is meaningful.  For
                     !  example, a 5 hPa zap_close_levels makes sense at 850 hPa.  However, a 5 hPa
                     !  critical thickness is sill when the top few isobaric levels are 1, 2, 3 hPa.
   
                     IF ( ordered_porig(ko) .GT. zap_close_levels * 10 ) THEN
   
                        !  Now we have a grid point that we should remove.  We pull out the pressure
                        !  and field values, then we drop the rest of the array to fill in the
                        !  missing spot, we increment our counter of bad values found in this column,
                        !  and then we reduce the count of the total number of values in the array.
     
                        DO kn = ko+1 , kinterp_end
                           ordered_porig(kn-1) = ordered_porig(kn)
                           ordered_forig(kn-1) = ordered_forig(kn)
                        END DO
                        final_zap_check_count = final_zap_check_count + 1
                     END IF
                  END IF

                  !  Did we pull down another pressure difference into the ko and ko+1 slots that will
                  !  cause troubles?  Make sure we don't spend an infinite amount of time in this loop.

                  IF ( ( ABS(ordered_porig(ko) - ordered_porig(ko+1)) .GE. zap_close_levels ) .OR. &
                       ( ordered_porig(ko) .LE. zap_close_levels * 10 ) ) THEN
                     EXIT close_by_at_ko
                  ELSE IF ( count_close_by_at_ko .GT. 3 ) THEN
                     final_zap_check_count = 99
                     EXIT close_by_at_ko
                  ELSE
                     count_close_by_at_ko = count_close_by_at_ko + 1
                     CYCLE close_by_at_ko
                  END IF
               END DO close_by_at_ko
            END DO
            IF ( final_zap_check_count .GT. 2 ) THEN
               WRITE ( message , * ) 'We are removing too many values: ',final_zap_check_count,' for (i,j) = ',i,j
               CALL wrf_error_fatal ( TRIM(message) )
            END IF
            kinterp_end = kinterp_end - final_zap_check_count
#else
            outer : DO ko = kinterp_start , kinterp_end-1
               IF ( ( ABS(ordered_porig(ko) - ordered_porig(ko+1)) .LT. MAX(zap_close_levels/10,50.) ) .AND. &
                    ( ordered_porig(ko) .GT. zap_close_levels * 10 ) ) THEN
                  WRITE ( message , FMT='(a,I2.2,a,F9.2,a,F9.2,a,i4,a,i4,a,a)' ) '*** -> Check your wrfinput_d',id, &
                                                                    ' file, you might have input pressure levels too close together (',&
                                                                    ordered_porig(ko),' Pa and ', ordered_porig(ko+1), &
                                                                    ' Pa) at (',i,',',j,') for variable type ',var_type
                     CALL wrf_message ( TRIM(message) )
                  EXIT outer
               END IF
            END DO outer
#endif

            !  The polynomials are either in pressure or LOG(pressure).

            IF ( interp_type .EQ. 1 ) THEN
               CALL lagrange_setup ( var_type , interp_type , &
                    ordered_porig(kinterp_start:kinterp_end) , &
                    ordered_forig(kinterp_start:kinterp_end) , &
                    kinterp_end-kinterp_start+1 , lagrange_order , extrap_type , &
                    ordered_pnew(kstart:kend)  , ordered_fnew  , kend-kstart+1 ,i,j)
            ELSE
               CALL lagrange_setup ( var_type , interp_type , &
                LOG(ordered_porig(kinterp_start:kinterp_end)) , &
                    ordered_forig(kinterp_start:kinterp_end) , &
                    kinterp_end-kinterp_start+1 , lagrange_order , extrap_type , &
                LOG(ordered_pnew(kstart:kend)) , ordered_fnew  , kend-kstart+1 ,i,j)
            END IF

            !  Save the computed data.

            DO kn = kstart , kend
               fnew(i,kn,j) = ordered_fnew(kn)
            END DO

            !  There may have been a request to have the surface data from the input field
            !  to be assigned as to the lowest eta level.  This assumes thin layers (usually
            !  the isobaric original field has the surface from 2-m T and RH, and 10-m U and V).

            IF ( lowest_lev_from_sfc ) THEN
               fnew(i,1,j) = forig(i,1,j)
            END IF

         END DO

      END DO

   END SUBROUTINE vert_interp

!---------------------------------------------------------------------

   SUBROUTINE lagrange_setup ( var_type , interp_type , all_x , all_y , all_dim , n , extrap_type , &
                               target_x , target_y , target_dim ,i,j)

      !  We call a Lagrange polynomial interpolator.  The parallel concerns are put off as this
      !  is initially set up for vertical use.  The purpose is an input column of pressure (all_x),
      !  and the associated pressure level data (all_y).  These are assumed to be sorted (ascending
      !  or descending, no matter).  The locations to be interpolated to are the pressures in
      !  target_x, probably the new vertical coordinate values.  The field that is output is the
      !  target_y, which is defined at the target_x location.  Mostly we expect to be 2nd order
      !  overlapping polynomials, with only a single 2nd order method near the top and bottom.
      !  When n=1, this is linear; when n=2, this is a second order interpolator.

      IMPLICIT NONE

      CHARACTER (LEN=1) :: var_type
      INTEGER , INTENT(IN) :: interp_type , all_dim , n , extrap_type , target_dim
      REAL, DIMENSION(all_dim) , INTENT(IN) :: all_x , all_y
      REAL , DIMENSION(target_dim) , INTENT(IN) :: target_x
      REAL , DIMENSION(target_dim) , INTENT(OUT) :: target_y

!  cubic spline defs
      INTEGER :: K
      REAL :: DX, ALPHA, BETA, GAMMA, ETA
      REAL , DIMENSION(all_dim) :: P2
!  cubic spline defs

      !  Brought in for debug purposes, all of the computations are in a single column.

      INTEGER , INTENT(IN) :: i,j

      !  Local vars

      REAL , DIMENSION(n+1) :: x , y
      REAL :: a , b
      REAL :: target_y_1 , target_y_2
      LOGICAL :: found_loc
      INTEGER :: loop , loc_center_left , loc_center_right , ist , iend , target_loop
      INTEGER :: vboundb , vboundt

      !  Local vars for the problem of extrapolating theta below ground.

      REAL :: temp_1 , temp_2 , temp_3 , temp_y
      REAL :: depth_of_extrap_in_p , avg_of_extrap_p , temp_extrap_starting_point , dhdp , dh , dt
#ifdef VERT_UNIT
      REAL , PARAMETER :: RovCp      = 0.287
#else
      REAL , PARAMETER :: RovCp      = rcp
#endif
      REAL , PARAMETER :: CRC_const1 = 11880.516      ! m
      REAL , PARAMETER :: CRC_const2 =     0.1902632  !
      REAL , PARAMETER :: CRC_const3 =     0.0065     ! K/km
      REAL, DIMENSION(all_dim) :: all_x_full
      REAL , DIMENSION(target_dim) :: target_x_full

      IF ( all_dim .LT. n+1 ) THEN
print *,'all_dim = ',all_dim
print *,'order = ',n
print *,'i,j = ',i,j
print *,'p array = ',all_x
print *,'f array = ',all_y
print *,'p target= ',target_x
         CALL wrf_message ( 0 , 'Troubles, the interpolating order is too large for this few input values' )
         CALL wrf_message ( 0 , 'This is usually caused by bad pressures' )
         CALL wrf_message ( 0 , 'At this (i,j), look at the input value of pressure from metgrid' )
         CALL wrf_message ( 0 , 'The surface pressure and the sea-level pressure should be reviewed, also from metgrid' )
         CALL wrf_message ( 0 , 'Finally, ridiculous values of moisture can mess up the vertical pressures, especially aloft' )
         CALL wrf_message ( 0 , 'The variable type is ' // var_type // '. This is not a unique identifer, but a type of field' )
         CALL wrf_message ( 0 , 'Check to see if all time periods with this data fail, or just this one' )
         CALL wrf_error_fatal ( 'This vertical interpolation failure is more typically associated with untested data sources to ungrib' )
      END IF

      IF ( n .LT. 1 ) THEN
         CALL wrf_error_fatal ( 'pal, linear is about as low as we go' )
      END IF

      !  We can pinch in the area of the higher order interpolation with vbound.  If
      !  vbound = 0, no pinching.  If vbound = m, then we make the lower "m" and upper
      !  "m" eta levels use a linear interpolation.

      vboundb = 4
      vboundt = 0

      !  Loop over the list of target x and y values.

      DO target_loop = 1 , target_dim

         !  Find the two trapping x values, and keep the indices.

         found_loc = .FALSE.
         find_trap : DO loop = 1 , all_dim -1
            a = target_x(target_loop) - all_x(loop)
            b = target_x(target_loop) - all_x(loop+1)
            IF ( a*b .LE. 0.0 ) THEN
               loc_center_left  = loop
               loc_center_right = loop+1
               found_loc = .TRUE.
               EXIT find_trap
            END IF
         END DO find_trap

         IF ( ( .NOT. found_loc ) .AND. ( target_x(target_loop) .GT. all_x(1) ) ) THEN

            !  Get full pressure back so that our extrpolations make sense.

            IF ( interp_type .EQ. 1 ) THEN
               all_x_full    =       all_x
               target_x_full =       target_x
            ELSE
               all_x_full    = EXP ( all_x )
               target_x_full = EXP ( target_x )
            END IF
            !  Isothermal extrapolation.

            IF      ( ( extrap_type .EQ. 1 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               temp_1 = all_y(1) * ( all_x_full(1) / 100000. ) ** RovCp
               target_y(target_loop) = temp_1 * ( 100000. / target_x_full(target_loop) ) ** RovCp

            !  Standard atmosphere -6.5 K/km lapse rate for the extrapolation.

            ELSE IF ( ( extrap_type .EQ. 2 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               depth_of_extrap_in_p = target_x_full(target_loop) - all_x_full(1)
               avg_of_extrap_p = ( target_x_full(target_loop) + all_x_full(1) ) * 0.5
               temp_extrap_starting_point = all_y(1) * ( all_x_full(1) / 100000. ) ** RovCp
               dhdp = CRC_const1 * CRC_const2 * ( avg_of_extrap_p / 100. ) ** ( CRC_const2 - 1. )
               dh = dhdp * ( depth_of_extrap_in_p / 100. )
               dt = dh * CRC_const3
               target_y(target_loop) = ( temp_extrap_starting_point + dt ) * ( 100000. / target_x_full(target_loop) ) ** RovCp

            !  Adiabatic extrapolation for theta.

            ELSE IF ( ( extrap_type .EQ. 3 ) .AND. ( var_type .EQ. 'T' ) ) THEN

               target_y(target_loop) = all_y(1)


            !  Wild extrapolation for non-temperature vars.

            ELSE IF ( extrap_type .EQ. 1 ) THEN

               target_y(target_loop) = ( all_y(2) * ( target_x(target_loop) - all_x(3) ) + &
                                         all_y(3) * ( all_x(2) - target_x(target_loop) ) ) / &
                                       ( all_x(2) - all_x(3) )

            !  Use a constant value below ground.

            ELSE IF ( extrap_type .EQ. 2 ) THEN

               target_y(target_loop) = all_y(1)

            ELSE IF ( extrap_type .EQ. 3 ) THEN
               CALL wrf_error_fatal ( 'You are not allowed to use extrap_option #3 for any var except for theta.' )

            END IF
            CYCLE
         ELSE IF ( .NOT. found_loc ) THEN
            print *,'i,j = ',i,j
            print *,'target pressure and value = ',target_x(target_loop),target_y(target_loop)
            DO loop = 1 , all_dim
               print *,'column of pressure and value = ',all_x(loop),all_y(loop)
            END DO
            CALL wrf_error_fatal ( 'troubles, could not find trapping x locations' )
         END IF

         !  Even or odd order?  We can put the value in the middle if this is
         !  an odd order interpolator.  For the even guys, we'll do it twice
         !  and shift the range one index, then get an average.

         IF ( n .EQ. 9 ) THEN
            CALL cubic_spline (all_dim-1, all_x, all_y, P2)
!
! Find the value of function f(x)
!
          DX = all_x(loc_center_right) - all_x(loc_center_left)
          ALPHA = P2(loc_center_right)/(6*DX)
          BETA = -P2(loc_center_left)/(6*DX)
          GAMMA = all_y(loc_center_right)/DX - DX*P2(loc_center_right)/6
          ETA = DX*P2(loc_center_left)/6 - all_y(loc_center_left)/DX
          target_y(target_loop) = ALPHA*(target_x(target_loop)-all_x(loc_center_left))*(target_x(target_loop)-all_x(loc_center_left)) &
                                  *(target_x(target_loop)-all_x(loc_center_left)) &
                     +BETA*(target_x(target_loop)-all_x(loc_center_right))*(target_x(target_loop)-all_x(loc_center_right)) &
                                  *(target_x(target_loop)-all_x(loc_center_right)) &
                     +GAMMA*(target_x(target_loop)-all_x(loc_center_left)) &
                     +ETA*(target_x(target_loop)-all_x(loc_center_right))

         ELSE IF ( MOD(n,2) .NE. 0 ) THEN
            IF ( ( loc_center_left -(((n+1)/2)-1) .GE.       1 ) .AND. &
                 ( loc_center_right+(((n+1)/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n+1)/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop) )
            ELSE
               IF ( .NOT. found_loc ) THEN
                  CALL wrf_error_fatal ( 'I doubt this will happen, I will only do 2nd order for now' )
               END IF
            END IF

         ELSE IF ( ( MOD(n,2) .EQ. 0 ) .AND. &
                   ( ( target_loop .GE. 1 + vboundb ) .AND. ( target_loop .LE. target_dim - vboundt ) ) ) THEN
            IF      ( ( loc_center_left -(((n  )/2)-1) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)  ) .LE. all_dim ) .AND. &
                      ( loc_center_left -(((n  )/2)  ) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y_1              )
               ist  = loc_center_left -(((n  )/2)  )
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y_2              )
               target_y(target_loop) = ( target_y_1 + target_y_2 ) * 0.5

            ELSE IF ( ( loc_center_left -(((n  )/2)-1) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)  ) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)-1)
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop)   )
            ELSE IF ( ( loc_center_left -(((n  )/2)  ) .GE.       1 ) .AND. &
                      ( loc_center_right+(((n  )/2)-1) .LE. all_dim ) ) THEN
               ist  = loc_center_left -(((n  )/2)  )
               iend = ist + n
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , n , target_x(target_loop) , target_y(target_loop)   )
            ELSE
               CALL wrf_error_fatal ( 'unauthorized area, you should not be here' )
            END IF

         ELSE IF ( MOD(n,2) .EQ. 0 ) THEN
               ist  = loc_center_left
               iend = loc_center_right
               CALL lagrange_interp ( all_x(ist:iend) , all_y(ist:iend) , 1 , target_x(target_loop) , target_y(target_loop) )

         END IF

      END DO

   END SUBROUTINE lagrange_setup

!---------------------------------------------------------------------

! cubic spline routines

      SUBROUTINE cubic_spline (N, XI, FI, P2)
      !
      ! Function to carry out the cubic-spline approximation
      ! with the second-order derivatives returned.
      !
      INTEGER :: I
      INTEGER, INTENT (IN) :: N
      REAL, INTENT (IN), DIMENSION (N+1):: XI, FI
      REAL, INTENT (OUT), DIMENSION (N+1):: P2
      REAL, DIMENSION (N):: G, H
      REAL, DIMENSION (N-1):: D, B, C
!
! Assign the intervals and function differences
!
  DO I = 1, N
    H(I) = XI(I+1) - XI(I)
    G(I) = FI(I+1) - FI(I)
  END DO
!
! Evaluate the coefficient matrix elements
  DO I = 1, N-1
    D(I) = 2*(H(I+1)+H(I))
    B(I) = 6*(G(I+1)/H(I+1)-G(I)/H(I))
    C(I) = H(I+1)
  END DO
!
! Obtain the second-order derivatives
!
  CALL TRIDIAGONAL_LINEAR_EQ (N-1, D, C, C, B, G)
  P2(1) = 0
  P2(N+1) = 0
  DO I = 2, N
    P2(I) = G(I-1)
  END DO

END SUBROUTINE cubic_spline

!---------------------------------------------------------------------

    SUBROUTINE TRIDIAGONAL_LINEAR_EQ (L, D, E, C, B, Z)
!
! Function to solve the tridiagonal linear equation set.
!
  INTEGER, INTENT (IN) :: L
  INTEGER :: I
  REAL, INTENT (IN), DIMENSION (L):: D, E, C, B
  REAL, INTENT (OUT), DIMENSION (L):: Z
  REAL, DIMENSION (L):: Y, W
  REAL, DIMENSION (L-1):: V, T
!
! Evaluate the elements in the LU decomposition
!
  W(1) = D(1)
  V(1)  = C(1)
  T(1)  = E(1)/W(1)
  DO I = 2, L - 1
    W(I) = D(I)-V(I-1)*T(I-1)
    V(I) = C(I)
    T(I) = E(I)/W(I)
  END DO
  W(L) = D(L)-V(L-1)*T(L-1)
!
! Forward substitution to obtain y
!
  Y(1) = B(1)/W(1)
  DO I = 2, L
    Y(I) = (B(I)-V(I-1)*Y(I-1))/W(I)
  END DO
!
! Backward substitution to obtain z
  Z(L) = Y(L)
  DO I = L-1, 1, -1
    Z(I) = Y(I) - T(I)*Z(I+1)
  END DO

END SUBROUTINE TRIDIAGONAL_LINEAR_EQ

! end cubic spline routines

!---------------------------------------------------------------------

   SUBROUTINE lagrange_interp ( x , y , n , target_x , target_y )

      !  Interpolation using Lagrange polynomials.
      !  P(x) = f(x0)Ln0(x) + ... + f(xn)Lnn(x)
      !  where Lnk(x) = (x -x0)(x -x1)...(x -xk-1)(x -xk+1)...(x -xn)
      !                 ---------------------------------------------
      !                 (xk-x0)(xk-x1)...(xk-xk-1)(xk-xk+1)...(xk-xn)

      IMPLICIT NONE

      INTEGER , INTENT(IN) :: n
      REAL , DIMENSION(0:n) , INTENT(IN) :: x , y
      REAL , INTENT(IN) :: target_x

      REAL , INTENT(OUT) :: target_y

      !  Local vars

      INTEGER :: i , k
      REAL :: numer , denom , Px
      REAL , DIMENSION(0:n) :: Ln

      Px = 0.
      DO i = 0 , n
         numer = 1.
         denom = 1.
         DO k = 0 , n
            IF ( k .EQ. i ) CYCLE
            numer = numer * ( target_x  - x(k) )
            denom = denom * ( x(i)  - x(k) )
         END DO
         IF ( denom .NE. 0. ) THEN
            Ln(i) = y(i) * numer / denom
            Px = Px + Ln(i)
         ENDIF
      END DO
      target_y = Px

   END SUBROUTINE lagrange_interp

#ifndef VERT_UNIT
!---------------------------------------------------------------------

   SUBROUTINE p_dry ( mu0 , eta , pdht , pdry , full_levs , &
                             c3f , c3h , c4f , c4h ,             &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             its , ite , jts , jte , kts , kte )

   !  Compute reference pressure and the reference mu.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL :: full_levs

      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: mu0
      REAL , DIMENSION(        kms:kme        ) , INTENT(IN)     :: eta
      REAL , DIMENSION(        kms:kme        ) , INTENT(IN)     :: c3f , c3h , c4f , c4h
      REAL                                                       :: pdht
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pdry

      !  Local vars

      INTEGER :: i , j , k
      REAL , DIMENSION(        kms:kme        )                  :: eta_h

      IF ( full_levs ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     pdry(i,k,j) = c3f(k) * MU0(i,j) + c4f(k) + pdht
               END DO
            END DO
         END DO
      ELSE
         DO k = kts , kte-1
            eta_h(k) = ( eta(k) + eta(k+1) ) * 0.5
         END DO
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte-1
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                     pdry(i,k,j) = c3h(k) * MU0(i,j) + c4h(k) + pdht
               END DO
            END DO
         END DO
      END IF

   END SUBROUTINE p_dry

!---------------------------------------------------------------------

   SUBROUTINE p_dts ( pdts , intq , psfc , p_top , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute difference between the dry, total surface pressure and the top pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p_top
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)     :: psfc
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN)     :: intq
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(OUT)    :: pdts

      !  Local vars

      INTEGER :: i , j , k

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            pdts(i,j) = psfc(i,j) - intq(i,j) - p_top
         END DO
      END DO

   END SUBROUTINE p_dts

!---------------------------------------------------------------------

   SUBROUTINE p_dhs ( pdhs , ht , p0 , t0 , a , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute dry, hydrostatic surface pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(IN)     :: ht
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: pdhs

      REAL , INTENT(IN) :: p0 , t0 , a

      !  Local vars

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            pdhs(i,j) = p0 * EXP ( -t0/a + SQRT ( (t0/a)**2 - 2. * g * ht(i,j)/(a * Rd) ) )
         END DO
      END DO

   END SUBROUTINE p_dhs

!---------------------------------------------------------------------

   SUBROUTINE find_p_top ( p , p_top , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

   !  Find the largest pressure in the top level.  This is our p_top.  We are
   !  assuming that the top level is the location where the pressure is a minimum
   !  for each column.  In cases where the top surface is not isobaric, a
   !  communicated value must be shared in the calling routine.  Also in cases
   !  where the top surface is not isobaric, care must be taken that the new
   !  maximum pressure is not greater than the previous value.  This test is
   !  also handled in the calling routine.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL :: p_top
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN) :: p

      !  Local vars

      INTEGER :: i , j , k, min_lev

      i = its
      j = jts
      p_top = p(i,2,j)
      min_lev = 2
      DO k = 2 , kte
         IF ( p_top .GT. p(i,k,j) ) THEN
            p_top = p(i,k,j)
            min_lev = k
         END IF
      END DO

      k = min_lev
      p_top = p(its,k,jts)
      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            p_top = MAX ( p_top , p(i,k,j) )
         END DO
      END DO

   END SUBROUTINE find_p_top

!---------------------------------------------------------------------

   SUBROUTINE t_to_theta ( t , p , p00 , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute potential temperature from temperature and pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p00
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: t

      !  Local vars

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               t(i,k,j) = t(i,k,j) * ( p00 / p(i,k,j) ) ** (Rd / Cp)
            END DO
         END DO
      END DO

   END SUBROUTINE t_to_theta


!---------------------------------------------------------------------

   SUBROUTINE theta_to_t ( t , p , p00 , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Compute temperature from potential temp and pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: p00
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: t

      !  Local vars

      INTEGER :: i , j , k

      REAL , PARAMETER :: Rd = r_d
      CHARACTER (LEN=80) :: mess

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte-1
            DO i = its , MIN (ide-1 , ite )
             IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
             if ( p(i,k,j) .NE. 0. ) then
               t(i,k,j) = t(i,k,j) / ( ( p00 / p(i,k,j) ) ** (Rd / Cp) )
             else
               WRITE(mess,*) 'Troubles in theta_to_t'
               CALL wrf_debug(0,mess)
               WRITE(mess,*) "i,j,k = ", i,j,k
               CALL wrf_debug(0,mess)
               WRITE(mess,*) "p(i,k,j) = ", p(i,k,j)
               CALL wrf_debug(0,mess)
               WRITE(mess,*) "t(i,k,j) = ", t(i,k,j)
               CALL wrf_debug(0,mess)
             endif
            END DO
         END DO
      END DO

   END SUBROUTINE theta_to_t

!---------------------------------------------------------------------

   SUBROUTINE integ_moist ( q_in , p_in , pd_out , t_in , ght_in , intq , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

   !  Integrate the moisture field vertically.  Mostly used to get the total
   !  vapor pressure, which can be subtracted from the total pressure to get
   !  the dry pressure.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: q_in , p_in , t_in , ght_in
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: pd_out
      REAL , DIMENSION(ims:ime,        jms:jme) , INTENT(OUT)    :: intq

      !  Local vars

      INTEGER :: i , j , k
      INTEGER , DIMENSION(ims:ime) :: level_above_sfc
      REAL , DIMENSION(ims:ime,jms:jme) :: psfc , tsfc , qsfc, zsfc
      REAL , DIMENSION(ims:ime,kms:kme) :: q , p , t , ght, pd

      REAL :: rhobar , qbar , dz
      REAL :: p1 , p2 , t1 , t2 , q1 , q2 , z1, z2

      LOGICAL :: upside_down
      LOGICAL :: already_assigned_upside_down

      REAL , PARAMETER :: Rd = r_d

      !  Is the data upside down?


      already_assigned_upside_down = .FALSE.
      find_valid : DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            IF ( p_in(i,kts+1,j) .LT. p_in(i,kte,j) ) THEN
               upside_down = .TRUE.
               already_assigned_upside_down = .TRUE.
            ELSE
               upside_down = .FALSE.
               already_assigned_upside_down = .TRUE.
            END IF
            EXIT find_valid
         END DO
      END DO find_valid

      IF ( .NOT. already_assigned_upside_down ) THEN
         upside_down = .FALSE.
      END IF

      !  Get a surface value, always the first level of a 3d field.

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            psfc(i,j) = p_in(i,kts,j)
            tsfc(i,j) = t_in(i,kts,j)
            qsfc(i,j) = q_in(i,kts,j)
            zsfc(i,j) = ght_in(i,kts,j)
         END DO
      END DO

      DO j = jts , MIN ( jde-1 , jte )

         !  Initialize the integrated quantity of moisture to zero.

         DO i = its , MIN (ide-1 , ite )
            intq(i,j) = 0.
         END DO

         IF ( upside_down ) THEN
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               p(i,kts) = p_in(i,kts,j)
               t(i,kts) = t_in(i,kts,j)
               q(i,kts) = q_in(i,kts,j)
               ght(i,kts) = ght_in(i,kts,j)
               DO k = kts+1,kte
                  p(i,k) = p_in(i,kte+2-k,j)
                  t(i,k) = t_in(i,kte+2-k,j)
                  q(i,k) = q_in(i,kte+2-k,j)
                  ght(i,k) = ght_in(i,kte+2-k,j)
               END DO
            END DO
         ELSE
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               DO k = kts,kte
                  p(i,k) = p_in(i,k      ,j)
                  t(i,k) = t_in(i,k      ,j)
                  q(i,k) = q_in(i,k      ,j)
                  ght(i,k) = ght_in(i,k      ,j)
               END DO
            END DO
         END IF

         !  Find the first level above the ground.  If all of the levels are above ground, such as
         !  a terrain following lower coordinate, then the first level above ground is index #2.

         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            level_above_sfc(i) = -1
            IF ( p(i,kts+1) .LT. psfc(i,j) ) THEN
               level_above_sfc(i) = kts+1
            ELSE
               find_k : DO k = kts+1,kte-1
                  IF ( ( p(i,k  )-psfc(i,j) .GE. 0. ) .AND. &
                       ( p(i,k+1)-psfc(i,j) .LT. 0. ) ) THEN
                     level_above_sfc(i) = k+1
                     EXIT find_k
                  END IF
               END DO find_k
               IF ( level_above_sfc(i) .EQ. -1 ) THEN
print *,'i,j = ',i,j
print *,'p = ',p(i,:)
print *,'p sfc = ',psfc(i,j)
                  CALL wrf_error_fatal ( 'Could not find level above ground')
               END IF
            END IF
         END DO

         DO i = its , MIN (ide-1 , ite )
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

            !  Account for the moisture above the ground.

            pd(i,kte) = p(i,kte)
            DO k = kte-1,level_above_sfc(i),-1
                  rhobar = ( p(i,k  ) / ( Rd * t(i,k  ) ) + &
                             p(i,k+1) / ( Rd * t(i,k+1) ) ) * 0.5
                  qbar   = ( q(i,k  ) + q(i,k+1) ) * 0.5
                  dz     = ght(i,k+1) - ght(i,k)
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
                  pd(i,k) = p(i,k) - intq(i,j)
            END DO

            !  Account for the moisture between the surface and the first level up.

            IF ( ( p(i,level_above_sfc(i)-1)-psfc(i,j) .GE. 0. ) .AND. &
                 ( p(i,level_above_sfc(i)  )-psfc(i,j) .LT. 0. ) .AND. &
                 ( level_above_sfc(i) .GT. kts ) ) THEN
               p1 = psfc(i,j)
               p2 = p(i,level_above_sfc(i))
               t1 = tsfc(i,j)
               t2 = t(i,level_above_sfc(i))
               q1 = qsfc(i,j)
               q2 = q(i,level_above_sfc(i))
               z1 = zsfc(i,j)
               z2 = ght(i,level_above_sfc(i))
               rhobar = ( p1 / ( Rd * t1 ) + &
                          p2 / ( Rd * t2 ) ) * 0.5
               qbar   = ( q1 + q2 ) * 0.5
               dz     = z2 - z1
               IF ( dz .GT. 0.1 ) THEN
                  intq(i,j) = intq(i,j) + g * qbar * rhobar / (1. + qbar) * dz
               END IF

               !  Fix the underground values.

               DO k = level_above_sfc(i)-1,kts+1,-1
                  pd(i,k) = p(i,k) - intq(i,j)
               END DO
            END IF
            pd(i,kts) = psfc(i,j) - intq(i,j)

         END DO

         IF ( upside_down ) THEN
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               pd_out(i,kts,j) = pd(i,kts)
               DO k = kts+1,kte
                  pd_out(i,kte+2-k,j) = pd(i,k)
               END DO
            END DO
         ELSE
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               DO k = kts,kte
                  pd_out(i,k,j) = pd(i,k)
               END DO
            END DO
         END IF

      END DO

   END SUBROUTINE integ_moist

!---------------------------------------------------------------------

   SUBROUTINE rh_to_mxrat2(rh, t, p, q , wrt_liquid , &
                           qv_max_p_safe , &
                           qv_max_flag , qv_max_value , &
                           qv_min_p_safe , &
                           qv_min_flag , qv_min_value , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      !  This subroutine computes mixing ratio (q, kg/kg) from basic variables
      !  pressure (p, Pa), temperature (t, K) and relative humidity (rh, 0-100%).
      !  Phase transition, liquid water to ice, occurs over (0,-23) temperature range (Celcius).
      !  Formulation used here is based on:
      !  WMO, General meteorological standards and recommended practices,
      !   Appendix A, WMO Technical Regulations, WMO-No. 49, corrigendum,
      !   August 2000.    --TKW 03/30/2011

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

      REAL , INTENT(IN)           :: qv_max_p_safe , qv_max_flag , qv_max_value
      REAL , INTENT(IN)           :: qv_min_p_safe , qv_min_flag , qv_min_value

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: q

      !  Local vars

      REAL,         PARAMETER     :: T0K = 273.16
      REAL,         PARAMETER     :: Tice = T0K - 23.0

      REAL,         PARAMETER     :: cfe = 1.0/(23.0*23.0)
      REAL,         PARAMETER     :: eps = 0.622

      ! Coefficients for esat over liquid water
      REAL,         PARAMETER     :: cw1 = 10.79574
      REAL,         PARAMETER     :: cw2 = -5.02800
      REAL,         PARAMETER     :: cw3 = 1.50475E-4
      REAL,         PARAMETER     :: cw4 = 0.42873E-3
      REAL,         PARAMETER     :: cw5 = 0.78614

      ! Coefficients for esat over ice
      REAL,         PARAMETER     :: ci1 = -9.09685
      REAL,         PARAMETER     :: ci2 = -3.56654
      REAL,         PARAMETER     :: ci3 = 0.87682
      REAL,         PARAMETER     :: ci4 = 0.78614

      REAL,         PARAMETER     :: Tn = 273.16

      ! 1 ppm is a reasonable estimate for minimum QV even for stratospheric altitudes
      REAL,         PARAMETER     :: QV_MIN = 1.e-6

      ! Maximum allowed QV is computed under the extreme condition:
      !            Saturated at 40 degree in Celcius and 1000 hPa
      REAL,         PARAMETER     :: QV_MAX = 0.045

      ! Need to constrain WVP in the stratosphere where pressure
      ! is low but tempearure is hot (warm)
      ! Maximum ratio of e/p, = q/(0.622+q)
      REAL,         PARAMETER     :: EP_MAX = QV_MAX/(eps+QV_MAX)

      INTEGER                     :: i , j , k

      REAL                        :: ew , q1 , t1
      REAL                        :: ta, tb, pw3, pw4, pwr
      REAL                        :: es, esw, esi, wvp, pmb, wvpmax

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               rh(i,k,j) = MIN ( MAX ( rh(i,k,j) ,  0. ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  Ta=Tn/T(i,k,j)
                  Tb=T(i,k,j)/Tn
                  pw3 = -8.2969*(Tb-1.0)
                  pw4 = 4.76955*(1.0-Ta)
                  pwr = cw1*(1.0-Ta) + cw2*LOG10(Tb) + cw3*(1.0-10.0**pw3) + cw4*(10.0**pw4-1.0) + cw5
                  es = 10.0**pwr             ! Saturation WVP
                  wvp = 0.01*rh(i,k,j)*es    ! Actual WVP
                  pmb = p(i,k,j)/100.
                  wvpmax = EP_MAX*pmb        ! Prevents unrealistic QV in the stratosphere
                  wvp = MIN(wvp,wvpmax)
                  q(i,k,j) = eps*wvp/(pmb-wvp)
               END DO
            END DO
         END DO

      ELSE
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  Ta=Tn/T(i,k,j)
                  Tb=T(i,k,j)/Tn
                  IF (t(i,k,j) >= T0K) THEN         ! Over liquid water
                     pw3 = -8.2969*(Tb-1.0)
                     pw4 = 4.76955*(1.0-Ta)
                     pwr = cw1*(1.0-Ta) + cw2*LOG10(Tb) + cw3*(1.0-10.0**pw3) + cw4*(10.0**pw4-1.0) + cw5
                     es = 10.0**pwr
                     wvp = 0.01*rh(i,k,j)*es
                  ELSE IF (t(i,k,j) <= Tice) THEN   ! Over ice
                     pwr = ci1*(Ta-1.0) + ci2*LOG10(Ta) + ci3*(1.0-Tb) + ci4
                     es = 10.0**pwr
                     wvp = 0.01*rh(i,k,j)*es
                  ELSE                              ! Mixed
                     pw3 = -8.2969*(Tb-1.0)
                     pw4 = 4.76955*(1.0-Ta)
                     pwr = cw1*(1.0-Ta) + cw2*LOG10(Tb) + cw3*(1.0-10.0**pw3) + cw4*(10.0**pw4-1.0) + cw5
                     esw = 10.0**pwr      ! Over liquid water

                     pwr = ci1*(Ta-1.0) + ci2*LOG10(Ta) + ci3*(1.0-Tb) + ci4
                     esi = 10.0**pwr      ! Over ice

                     es = esi + (esw-esi)*cfe*(T(i,k,j)-Tice)*(T(i,k,j)-Tice)
                     wvp = 0.01*rh(i,k,j)*es
                  END IF
                  pmb = p(i,k,j)/100.
                  wvpmax = EP_MAX*pmb     ! Prevents unrealistic QV in the stratosphere
                  wvp = MIN(wvp,wvpmax)
                  q(i,k,j) = eps*wvp/(pmb-wvp)
               END DO
            END DO
         END DO
      END IF

      !  For pressures above a defined level, reasonable Qv values should be
      !  a certain value or smaller.  If they are larger than this, the input data
      !  probably had "missing" RH, and we filled in some values.  This is an
      !  attempt to catch those.  Also, set the minimum value for the entire
      !  domain that is above the selected pressure level.
     
      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF ( p(i,k,j) .LT. qv_max_p_safe ) THEN
                  IF ( q(i,k,j) .GT. qv_max_flag ) THEN
                     q(i,k,j) = qv_max_value
                  END IF
               END IF
               IF ( p(i,k,j) .LT. qv_min_p_safe ) THEN
                  IF ( q(i,k,j) .LT. qv_min_flag ) THEN
                     q(i,k,j) = qv_min_value
                  END IF
               END IF
            END DO
         END DO
      END DO

   END SUBROUTINE rh_to_mxrat2

!---------------------------------------------------------------------

   SUBROUTINE rh_to_mxrat1(rh, t, p, q , wrt_liquid , &
                           qv_max_p_safe , &
                           qv_max_flag , qv_max_value , &
                           qv_min_p_safe , &
                           qv_min_flag , qv_min_value , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      LOGICAL , INTENT(IN)        :: wrt_liquid

      REAL , INTENT(IN)           :: qv_max_p_safe , qv_max_flag , qv_max_value
      REAL , INTENT(IN)           :: qv_min_p_safe , qv_min_flag , qv_min_value

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: p , t
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: rh
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(OUT)    :: q

      !  Local vars

      INTEGER                     :: i , j , k

      REAL                        :: ew , q1 , t1

      REAL,         PARAMETER     :: T_REF       = 0.0
      REAL,         PARAMETER     :: MW_AIR      = 28.966
      REAL,         PARAMETER     :: MW_VAP      = 18.0152

      REAL,         PARAMETER     :: A0       = 6.107799961
      REAL,         PARAMETER     :: A1       = 4.436518521e-01
      REAL,         PARAMETER     :: A2       = 1.428945805e-02
      REAL,         PARAMETER     :: A3       = 2.650648471e-04
      REAL,         PARAMETER     :: A4       = 3.031240396e-06
      REAL,         PARAMETER     :: A5       = 2.034080948e-08
      REAL,         PARAMETER     :: A6       = 6.136820929e-11

      REAL,         PARAMETER     :: ES0 = 6.1121

      REAL,         PARAMETER     :: C1       = 9.09718
      REAL,         PARAMETER     :: C2       = 3.56654
      REAL,         PARAMETER     :: C3       = 0.876793
      REAL,         PARAMETER     :: EIS      = 6.1071
      REAL                        :: RHS
      REAL,         PARAMETER     :: TF       = 273.16
      REAL                        :: TK

      REAL                        :: ES
      REAL                        :: QS
      REAL,         PARAMETER     :: EPS         = 0.622
      REAL,         PARAMETER     :: SVP1        = 0.6112
      REAL,         PARAMETER     :: SVP2        = 17.67
      REAL,         PARAMETER     :: SVP3        = 29.65
      REAL,         PARAMETER     :: SVPT0       = 273.15

      CHARACTER (LEN=80) :: mess

      !  This subroutine computes mixing ratio (q, kg/kg) from basic variables
      !  pressure (p, Pa), temperature (t, K) and relative humidity (rh, 1-100%).
      !  The reference temperature (t_ref, C) is used to describe the temperature
      !  at which the liquid and ice phase change occurs.

      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               rh(i,k,j) = MIN ( MAX ( rh(i,k,j) ,  0. ) , 100. )
            END DO
         END DO
      END DO

      IF ( wrt_liquid ) THEN
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

! es is reduced by RH here to avoid problems in low-pressure cases
                  if (t(i,k,j) .ne. 0.) then
                     es=.01*rh(i,k,j)*svp1*10.*EXP(svp2*(t(i,k,j)-svpt0)/(t(i,k,j)-svp3))
                     IF (es .ge. p(i,k,j)/100.)THEN
                       q(i,k,j)=1.E-6
                       WRITE(mess,*) 'Warning: vapor pressure exceeds total pressure, setting Qv to 1.E-6'
                       CALL wrf_debug(1,mess)
                     ELSE
                       q(i,k,j)=MAX(eps*es/(p(i,k,j)/100.-es),1.E-6)
                     ENDIF
                  else
                     q(i,k,j)=1.E-6
                     WRITE(mess,*) 't(i,j,k) was 0 at ', i,j,k,', setting Qv to 0'
                     CALL wrf_debug(0,mess)
                  endif
               END DO
            END DO
         END DO

      ELSE
         DO j = jts , MIN ( jde-1 , jte )
            DO k = kts , kte
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

                  t1 = t(i,k,j) - 273.16

                  !  Obviously dry.

                  IF ( t1 .lt. -200. ) THEN
                     q(i,k,j) = 0

                  ELSE

                     !  First compute the ambient vapor pressure of water

                     !  Liquid phase t > 0 C

                     IF ( t1 .GE. t_ref ) THEN
                        ew = a0 + t1 * (a1 + t1 * (a2 + t1 * (a3 + t1 * (a4 + t1 * (a5 + t1 * a6)))))

                     !  Mixed phase -47 C < t < 0 C

                     ELSE IF ( ( t1 .LT. t_ref ) .AND. ( t1 .GE. -47. ) ) THEN
                        ew = es0 * exp(17.67 * t1 / ( t1 + 243.5))

                     !  Ice phase t < -47 C

                     ELSE IF ( t1 .LT. -47. ) THEN
                        tk = t(i,k,j)
                        rhs = -c1 * (tf / tk - 1.) - c2 * alog10(tf / tk) +  &
                               c3 * (1. - tk / tf) +      alog10(eis)
                        ew = 10. ** rhs

                     END IF

                     !  Now sat vap pres obtained compute local vapor pressure

                     ew = MAX ( ew , 0. ) * rh(i,k,j) * 0.01

                     !  Now compute the specific humidity using the partial vapor
                     !  pressures of water vapor (ew) and dry air (p-ew).  The
                     !  constants assume that the pressure is in hPa, so we divide
                     !  the pressures by 100.

                     q1 = mw_vap * ew
                     q1 = q1 / (q1 + mw_air * (p(i,k,j)/100. - ew))

                     q(i,k,j) = q1 / (1. - q1 )

                  END IF

               END DO
            END DO
         END DO
      END IF

      !  For pressures above a defined level, reasonable Qv values should be
      !  a certain value or smaller.  If they are larger than this, the input data
      !  probably had "missing" RH, and we filled in some values.  This is an
      !  attempt to catch those.  Also, set the minimum value for the entire
      !  domain that is above the selected pressure level.
     
      DO j = jts , MIN ( jde-1 , jte )
         DO k = kts , kte
            DO i = its , MIN (ide-1 , ite )
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF ( p(i,k,j) .LT. qv_max_p_safe ) THEN
                  IF ( q(i,k,j) .GT. qv_max_flag ) THEN
                     q(i,k,j) = qv_max_value
                  END IF
               END IF
               IF ( p(i,k,j) .LT. qv_min_p_safe ) THEN
                  IF ( q(i,k,j) .LT. qv_min_flag ) THEN
                     q(i,k,j) = qv_min_value
                  END IF
               END IF
            END DO
         END DO
      END DO

   END SUBROUTINE rh_to_mxrat1

!---------------------------------------------------------------------

#if 0
program foo

!  Make this local variable have the same value as in 
!  frame/module_driver_constants.F: MAX_ETA
integer , parameter :: max_eta = 10001

INTEGER :: ids , ide , jds , jde , kds , kde , &
           ims , ime , jms , jme , kms , kme , &
           its , ite , jts , jte , kts , kte

real :: max_dz = 1000
real :: p_top = 100
real :: g = 9.81
real :: p00 = 100000
real :: cvpm = -0.714285731
real :: a = 50
real :: r_d = 287
real :: cp = 1004.5
real :: t00 = 290
real :: p1000mb = 100000
real :: t0 = 300
real :: tiso = 216.649994
real :: p_strat = 5500
real :: a_strat = -12

real , dimension(max_eta) :: znw , eta_levels

eta_levels = -1

kds=1
kms=1
kts=1
kde=70
kme=70
kte=70


call compute_eta ( znw , auto_levels_opt, &
                   eta_levels , max_eta , max_dz , dzbot , dzstretch_s , dzstretch_u , &
                   p_top , g , p00 , cvpm , a , r_d , cp , &
                   t00 , p1000mb , t0 , tiso , p_strat , a_strat , &
                   ids , ide , jds , jde , kds , kde , &
                   ims , ime , jms , jme , kms , kme , &
                   its , ite , jts , jte , kts , kte )

end program foo
#endif

   SUBROUTINE compute_eta ( znw , auto_levels_opt , &
                           eta_levels , max_eta , max_dz , dzbot , dzstretch_s , dzstretch_u , &
                           p_top , g , p00 , cvpm , a , r_d , cp , &
                           t00 , p1000mb , t0 , tiso , p_strat , a_strat , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           its , ite , jts , jte , kts , kte )

      !  Compute eta levels, either using given values from the namelist (hardly
      !  a computation, yep, I know), or assuming a constant dz above the PBL,
      !  knowing p_top and the number of eta levels.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte
      REAL , INTENT(IN)           :: max_dz, dzbot, dzstretch_s, dzstretch_u
      REAL , INTENT(IN)           :: p_top , g , p00 , cvpm , a , r_d , cp , t00 , p1000mb , t0 , tiso
      REAL , INTENT(IN)           :: p_strat , a_strat
      INTEGER , INTENT(IN)        :: max_eta, auto_levels_opt
      REAL , DIMENSION (max_eta)  :: eta_levels

      REAL , DIMENSION (kts:kte) , INTENT(OUT) :: znw

      !  Local vars

      INTEGER :: k , kk
      REAL(KIND=8) :: mub , t_init , p_surf , pb, ztop, ztop_pbl , dz , temp
      REAL(KIND=8) , DIMENSION(kts:kte) :: dnw

      INTEGER , PARAMETER :: prac_levels = 59
      INTEGER :: loop , loop1
      REAL(KIND=8) , DIMENSION(prac_levels) :: znw_prac , znu_prac , dnw_prac
      REAL(KIND=8) , DIMENSION(MAX(prac_levels,kde)) :: alb , phb
      REAL(KIND=8) :: alb_max, t_init_max, pb_max, phb_max
      REAL(KIND=8) :: p00_r8, t00_r8, a_r8, tiso_r8

      CHARACTER(LEN=256) :: message

      !  Gee, do the eta levels come in from the namelist?

      IF ( ABS(eta_levels(1)+1.) .GT. 0.0000001 ) THEN

         !  Check to see if the array is oriented OK, we can easily fix an upside down oops.

         IF      ( ( ABS(eta_levels(1  )-1.) .LT. 0.0000001 ) .AND. &
                   ( ABS(eta_levels(kde)-0.) .LT. 0.0000001 ) ) THEN
            DO k = kds+1 , kde-1
        znw(k) = eta_levels(k)
            END DO
            znw(  1) = 1.
            znw(kde) = 0.
         ELSE IF ( ( ABS(eta_levels(kde)-1.) .LT. 0.0000001 ) .AND. &
                   ( ABS(eta_levels(1  )-0.) .LT. 0.0000001 ) ) THEN
            DO k = kds+1 , kde-1
        znw(k) = eta_levels(kde+1-k)
            END DO
            znw(  1) = 1.
            znw(kde) = 0.
         ELSE
            CALL wrf_error_fatal ( 'First eta level should be 1.0 and the last 0.0 in namelist' )
         END IF

         !  Check to see if the input full-level eta array is monotonic.

         DO k = kds , kde-1
            IF ( znw(k) .LE. znw(k+1) ) THEN
               PRINT *,'eta on full levels is not monotonic'
               PRINT *,'eta (',k,') = ',znw(k)
               PRINT *,'eta (',k+1,') = ',znw(k+1)
               CALL wrf_error_fatal ( 'Fix non-monotonic "eta_levels" in the namelist.input file' )
            END IF
         END DO

      !  Compute eta levels assuming a constant delta z above the PBL.

      ELSE
       IF( auto_levels_opt == 1 ) THEN
         print *,'using old automatic levels program'
         !  Compute top of the atmosphere with some silly levels.  We just want to
         !  integrate to get a reasonable value for ztop.  We use the planned PBL-esque
         !  levels, and then just coarse resolution above that.  We know p_top, and we
         !  have the base state vars.

         p_surf = p00

         znw_prac = (/ 1.0000_8 , 0.9930_8 , 0.9830_8 , 0.9700_8 , 0.9540_8 , 0.9340_8 , 0.9090_8 , 0.8800_8 , &
                       0.8500_8 , 0.8000_8 , 0.7500_8 , 0.7000_8 , 0.6500_8 , 0.6000_8 , 0.5500_8 , 0.5000_8 , &
                       0.4500_8 , 0.4000_8 , 0.3500_8 , 0.3000_8 , 0.2500_8 , 0.2000_8 , 0.1500_8 , 0.1000_8 , &
                       0.0800_8 , 0.0600_8 , 0.0400_8 , 0.0200_8 , &
                       0.0150_8 , 0.0100_8 , 0.0090_8 , 0.0080_8 , 0.0070_8 , 0.0060_8 , 0.0050_8 , 0.0040_8 , &
                       0.0035_8 , 0.0030_8 , &
                       0.0028_8 , 0.0026_8 , 0.0024_8 , 0.0022_8 , 0.0020_8 , &
                       0.0018_8 , 0.0016_8 , 0.0014_8 , 0.0012_8 , 0.0010_8 , &
                       0.0009_8 , 0.0008_8 , 0.0007_8 , 0.0006_8 , 0.0005_8 , 0.0004_8 , 0.0003_8 , &
                       0.0002_8 , 0.0001_8 , 0.00005_8, 0.0000_8 /)

         DO k = 1 , prac_levels - 1
            znu_prac(k) = ( znw_prac(k) + znw_prac(k+1) ) * 0.5_8
            dnw_prac(k) = znw_prac(k+1) - znw_prac(k)
         END DO

         tiso_r8    = tiso
         t00_r8     = t00
         a_r8       = a
         p00_r8     = p00
         DO k = 1, prac_levels-1
            pb = znu_prac(k)*(p_surf - p_top) + p_top
            temp = MAX ( tiso_r8, t00_r8 + A_r8*LOG(pb/p00_r8) )
            IF ( pb .LT. p_strat ) THEN
                temp = tiso + A_strat*LOG(pb/p_strat)
            END IF
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
         END DO

         !  Base state mu is defined as base state surface pressure minus p_top

         mub = p_surf - p_top

         !  Integrate base geopotential, starting at terrain elevation.

         phb(1) = 0._8
         DO k  = 2,prac_levels
               phb(k) = phb(k-1) - dnw_prac(k-1)*mub*alb(k-1)
         END DO

         !  So, now we know the model top in meters.  Get the average depth above the PBL
         !  of each of the remaining levels.  We are going for a constant delta z thickness.

         ztop     = phb(prac_levels) / g
         ztop_pbl = phb(8          ) / g
         dz = ( ztop - ztop_pbl ) / REAL ( kde - 8 )

         IF ( dz .GE. max_dz ) THEN
            WRITE (message,FMT='("With a requested ",F7.1," Pa model top, the model lid will be about ",F7.1," m.")') p_top, ztop
            CALL wrf_message ( message )
            WRITE (message,FMT='("With ",I3," levels above the PBL, the level thickness will be about ",F6.1," m.")') kde-8, dz
            CALL wrf_message ( message )
            WRITE (message,FMT='("Thicknesses greater than ",F7.1," m are not recommended.")') max_dz
            CALL wrf_message ( message )
            CALL wrf_error_fatal ( 'Add more levels to namelist.input for e_vert' )
         END IF

         !  Standard levels near the surface so no one gets in trouble.

         DO k = 1 , 8
            eta_levels(k) = znw_prac(k)
         END DO

         !  Using d phb(k)/ d eta(k) = -mub * alb(k), eqn 2.9
         !  Skamarock et al, NCAR TN 468.  Use full levels, so
         !  use twice the thickness.

         DO k = 8, kte-1-2

            find_prac : DO kk = 1 , prac_levels
               IF (znw_prac(kk) .LT. eta_levels(k) ) THEN
                  EXIT find_prac
               END IF
            end do find_prac

            pb = 0.5*(eta_levels(k)+znw_prac(kk)) * (p_surf - p_top) + p_top

            temp = MAX ( tiso_r8, t00_r8 + A_r8*LOG(pb/p00_r8) )
            IF ( pb .LT. p_strat ) THEN
               temp = tiso + A_strat * LOG ( pb/p_strat )
            END IF
!           temp =             t00 + A*LOG(pb/p00)
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            eta_levels(k+1) = eta_levels(k) - dz*g / ( mub*alb(k) )
            pb = 0.5*(eta_levels(k)+eta_levels(k+1)) * (p_surf - p_top) + p_top

            temp = MAX ( tiso_r8, t00_r8 + A_r8*LOG(pb/p00_r8) )
            IF ( pb .LT. p_strat ) THEN
               temp = tiso + A_strat * LOG ( pb/p_strat )
            END IF
!           temp =             t00 + A*LOG(pb/p00)
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            eta_levels(k+1) = eta_levels(k) - dz*g / ( mub*alb(k) )
            pb = 0.5*(eta_levels(k)+eta_levels(k+1)) * (p_surf - p_top) + p_top

            phb(k+1) = phb(k) - (eta_levels(k+1)-eta_levels(k)) * mub*alb(k)
         END DO

         alb_max = alb(kte-1-2)
         t_init_max = t_init
         pb_max = pb
         phb_max = phb(kte-1)

         DO k = 1 , kte-1-2
            znw(k) = eta_levels(k)
         END DO
         znw(kte-2) = 0.000

         !  There is some iteration.  We want the top level, ztop, to be
         !  consistent with the delta z, and we want the half level values
         !  to be consistent with the eta levels.  The inner loop to 10 gets
         !  the eta levels very accurately, but has a residual at the top, due
         !  to dz changing.  We reset dz five times, and then things seem OK.

         DO loop1 = 1 , 5
            DO loop = 1 , 10
               DO k = 8, kte-1-2-1
                  pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
                  temp = MAX ( tiso_r8, t00_r8 + A_r8*LOG(pb/p00_r8) )
                  IF ( pb .LT. p_strat ) THEN
                     temp = tiso + A_strat * LOG ( pb/p_strat )
                  END IF
                  t_init = temp*(p00/pb)**(r_d/cp) - t0
                  alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
                  znw(k+1) = znw(k) - dz*g / ( mub*alb(k) )
               END DO
               pb = pb_max
               t_init = t_init_max
               alb(kte-1-2) = alb_max
               znw(kte-2) = znw(kte-1-2) - dz*g / ( mub*alb(kte-1-2) )
               IF ( ( loop1 .EQ. 5 ) .AND. ( loop .EQ. 10 ) ) THEN
                  print *,'Converged znw(kte) should be about 0.0 = ',znw(kte-2)
               END IF
               znw(kte-2) = 0.000
            END DO

            !  Here is where we check the eta levels values we just computed.

            DO k = 1, kde-1-2
               pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
               temp = MAX ( tiso_r8, t00_r8 + A_r8*LOG(pb/p00_r8) )
               IF ( pb .LT. p_strat ) THEN
                  temp = tiso + A_strat * LOG ( pb/p_strat )
               END IF
               t_init = temp*(p00/pb)**(r_d/cp) - t0
               alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            END DO

            phb(1) = 0.
            DO k  = 2,kde-2
               phb(k) = phb(k-1) - (znw(k)-znw(k-1)) * mub*alb(k-1)
            END DO

            !  Reset the model top and the dz, and iterate.

            ztop = phb(kde-2)/g
            ztop_pbl = phb(8)/g
            dz = ( ztop - ztop_pbl ) / REAL ( (kde-2) - 8 )
         END DO

         IF ( dz .GT. max_dz ) THEN
print *,'z (m)            = ',phb(1)/g
do k = 2 ,kte-2
print *,'z (m) and dz (m) = ',phb(k)/g,(phb(k)-phb(k-1))/g
end do
print *,'dz (m) above fixed eta levels = ',dz
print *,'namelist max_dz (m) = ',max_dz
print *,'namelist p_top (Pa) = ',p_top
            CALL wrf_debug ( 0, 'You need one of three things:' )
            CALL wrf_debug ( 0, '1) More eta levels to reduce the dz: e_vert' )
            CALL wrf_debug ( 0, '2) A lower p_top so your total height is reduced: p_top_requested')
            CALL wrf_debug ( 0, '3) Increase the maximum allowable eta thickness: max_dz')
            CALL wrf_debug ( 0, 'All are namelist options')
            CALL wrf_error_fatal ( 'dz above fixed eta levels is too large')
         END IF

         !  Add those 2 levels back into the middle, just above the 8 levels
         !  that semi define a boundary layer.  After we open up the levels,
         !  then we just linearly interpolate in znw.  So now levels 1-8 are
         !  specified as the fixed boundary layer levels given in this routine.
         !  The top levels, 12 through kte are those computed.  The middle
         !  levels 9, 10, and 11 are equi-spaced in znw, and are each 1/2 the
         !  the znw thickness of levels 11 through 12.

         DO k = kte-2 , 9 , -1
            znw(k+2) = znw(k)
         END DO

         znw( 9) = 0.75 * znw( 8) + 0.25 * znw(12)
         znw(10) = 0.50 * znw( 8) + 0.50 * znw(12)
         znw(11) = 0.25 * znw( 8) + 0.75 * znw(12)

         DO k = 8, kte-1
            pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
            temp = MAX ( tiso_r8, t00_r8 + A_r8*LOG(pb/p00_r8) )
            IF ( pb .LT. p_strat ) THEN
               temp = tiso + A_strat * LOG ( pb/p_strat )
            END IF
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            phb(k) = phb(k-1) - (znw(k)-znw(k-1)) * mub*alb(k-1)
         END DO
         phb(kte) = phb(kte-1) - (znw(kte)-znw(kte-1)) * mub*alb(kte-1)

       ELSE IF (auto_levels_opt == 2) THEN
         print *,'using new automatic levels program'
         CALL levels(kte-1, p_top, znw, max_dz, dzbot, dzstretch_s, dzstretch_u, r_d, g )
         p_surf = p00
         tiso_r8    = tiso
         t00_r8     = t00
         a_r8       = a
         p00_r8     = p00
         mub = p_surf - p_top
         phb(1) = 0.
         DO k = 1, kte-1
            pb = (znw(k)+znw(k+1))*0.5 * (p_surf - p_top) + p_top
            temp = MAX ( tiso_r8, t00_r8 + A_r8*LOG(pb/p00_r8) )
            IF ( pb .LT. p_strat ) THEN
               temp = tiso + A_strat * LOG ( pb/p_strat )
            END IF
            t_init = temp*(p00/pb)**(r_d/cp) - t0
            alb(k) = (r_d/p1000mb)*(t_init+t0)*(pb/p1000mb)**cvpm
            phb(k+1) = phb(k) - (znw(k+1)-znw(k)) * mub*alb(k)
         END DO
       ELSE
         print *,'auto_levels_opt=',auto_levels_opt
         CALL wrf_error_fatal ( 'auto_levels_opt needs to be 1 or 2')
       ENDIF
k=1
WRITE (*,FMT='("Full level index = ",I4,"     Height = ",F7.1," m")') k,phb(1)/g
do k = 2 ,kte
WRITE (*,FMT='("Full level index = ",I4,"     Height = ",F7.1," m      Thickness = ",F6.1," m")') k,phb(k)/g,(phb(k)-phb(k-1))/g
end do

      END IF

   END SUBROUTINE compute_eta

!---------------------------------------------------------------------
    SUBROUTINE levels ( nlev, ptop, eta, dzmax, dzbot, dzstretch_s, dzstretch_u, r_d, g )
    implicit none
    integer, intent(in) :: nlev
    real, intent(in) :: ptop, dzmax, dzbot, dzstretch_s, dzstretch_u, r_d, g
    real, dimension(0:nlev), intent(out) :: eta

    real, dimension(nlev) :: zup, pup
    real :: tt, a
    real :: ztop, dz, dztest, zscale
    integer :: isave, i

    tt=290. ! isothermal temperature used for z/log p relation - tt=290 fits dzbot
    ztop=r_d*tt/g*alog(1.e5/ptop)
    zscale=r_d*tt/g
    dz=dzbot
    zup(1)=dz
    pup(1)=1.e5*exp(-g*zup(1)/r_d/tt)
    eta(0)=1.0
    eta(1)=(pup(1)-ptop)/(1.e5-ptop)
    print *,1,dz,zup(1),eta(1)
    isave=1
    do i=1,nlev-1
        a=dzstretch_u+(dzstretch_s-dzstretch_u)*max((dzmax*0.5-dz)/(dzmax*0.5), 0.)
        dz=a*dz
        dztest=(ztop-zup(isave))/(nlev-isave)
        if(dztest.lt.dz)exit
        isave=i+1
        zup(i+1)=zup(i)+dz
        pup(i+1)=1.e5*exp(-g*zup(i+1)/r_d/tt)
        eta(i+1)=(pup(i+1)-ptop)/(1.e5-ptop)
        print *,i+1,dz,zup(i+1),eta(i+1),a
         IF ( i .EQ. nlev-1 ) THEN
            CALL wrf_debug ( 0, 'You need one of four things:' )
            CALL wrf_debug ( 0, '1) More eta levels: e_vert' )
            CALL wrf_debug ( 0, '2) A lower p_top: p_top_requested')
            CALL wrf_debug ( 0, '3) Increase the lowest eta thickness: dzbot')
            CALL wrf_debug ( 0, '4) Increase the stretching factor: dzstretch_s or dzstretch_u')
            CALL wrf_debug ( 0, 'All are namelist options')
            CALL wrf_error_fatal ( 'not enough eta levels to reach p_top')
         END IF
    enddo
    print *,ztop,zup(isave),nlev,isave
    dz=(ztop-zup(isave))/(nlev-isave)
         IF ( dz .GT. 1.5*dzmax ) THEN       ! isothermal temp 1.5 times stratosphere temp
            CALL wrf_debug ( 0, 'Warning: Upper levels may be too thick' )
            CALL wrf_debug ( 0, 'You need one of five things:' )
            CALL wrf_debug ( 0, '1) More eta levels: e_vert' )
            CALL wrf_debug ( 0, '2) A lower p_top: p_top_requested')
            CALL wrf_debug ( 0, '3) Increase the lowest eta thickness: dzbot')
            CALL wrf_debug ( 0, '4) Increase the stretching factor: dzstretch_s or dzstretch_u')
            CALL wrf_debug ( 0, '5) Increase the maximum allowed thickness: max_dz')
            CALL wrf_debug ( 0, 'All are namelist options')
            CALL wrf_error_fatal ( 'Upper levels may be too thick')
         END IF
    do i=isave,nlev-1
        zup(i+1)=zup(i)+dz
        pup(i+1)=1.e5*exp(-g*zup(i+1)/r_d/tt)
        eta(i+1)=(pup(i+1)-ptop)/(1.e5-ptop)
        print *,i+1,dz,zup(i+1),eta(i+1)
    enddo
    eta(nlev) = 0.
    print 1000, eta
    1000 format(10f10.4)
    !1000 format(10g10.3)
    return
    END SUBROUTINE levels

!---------------------------------------------------------------------

   SUBROUTINE monthly_min_max ( field_in , field_min , field_max , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Plow through each month, find the max, min values for each i,j.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,12,jms:jme) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_min , field_max

      !  Local vars

      INTEGER :: i , j , l
      REAL :: minner , maxxer

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            minner = field_in(i,1,j)
            maxxer = field_in(i,1,j)
            DO l = 2 , 12
               IF ( field_in(i,l,j) .LT. minner ) THEN
                  minner = field_in(i,l,j)
               END IF
               IF ( field_in(i,l,j) .GT. maxxer ) THEN
                  maxxer = field_in(i,l,j)
               END IF
            END DO
            field_min(i,j) = minner
            field_max(i,j) = maxxer
         END DO
      END DO

   END SUBROUTINE monthly_min_max

!---------------------------------------------------------------------

   SUBROUTINE monthly_interp_to_date ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Linrarly in time interpolate data to a current valid time.  The data is
   !  assumed to come in "monthly", valid at the 15th of every month.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,12,jms:jme) , INTENT(IN)  :: field_in
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j , l
      INTEGER , DIMENSION(0:13) :: middle
      INTEGER :: target_julyr , target_julday , target_date
      INTEGER :: julyr , julday , int_month , month1 , month2
      REAL :: gmt
      CHARACTER (LEN=4) :: yr
      CHARACTER (LEN=2) :: mon , day15


      WRITE(day15,FMT='(I2.2)') 15
      DO l = 1 , 12
         WRITE(mon,FMT='(I2.2)') l
         CALL get_julgmt ( date_str(1:4)//'-'//mon//'-'//day15//'_'//'00:00:00.0000' , julyr , julday , gmt )
         middle(l) = julyr*1000 + julday
      END DO

      l = 0
      middle(l) = middle( 1) - 31

      l = 13
      middle(l) = middle(12) + 31

      CALL get_julgmt ( date_str , target_julyr , target_julday , gmt )
      target_date = target_julyr * 1000 + target_julday
      find_month : DO l = 0 , 12
         IF ( ( middle(l) .LT. target_date ) .AND. ( middle(l+1) .GE. target_date ) ) THEN
            DO j = jts , MIN ( jde-1 , jte )
               DO i = its , MIN (ide-1 , ite )
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  int_month = l
                  IF ( ( int_month .EQ. 0 ) .OR. ( int_month .EQ. 12 ) ) THEN
                     month1 = 12
                     month2 =  1
                  ELSE
                     month1 = int_month
                     month2 = month1 + 1
                  END IF
                  field_out(i,j) =  ( field_in(i,month2,j) * ( target_date - middle(l)   ) + &
                                      field_in(i,month1,j) * ( middle(l+1) - target_date ) ) / &
                                    ( middle(l+1) - middle(l) )
               END DO
            END DO
            EXIT find_month
         END IF
      END DO find_month

   END SUBROUTINE monthly_interp_to_date

!---------------------------------------------------------------------

   SUBROUTINE eightday_selector ( field_in , date_str , field_out , &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )

   !  Given current date, select time-matching monthly entry from grid.
   !  No interpolation.

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      CHARACTER (LEN=24) , INTENT(IN) :: date_str
      REAL , DIMENSION(ims:ime,46,jms:jme) , INTENT(IN)  :: field_in  !46
      REAL , DIMENSION(ims:ime,   jms:jme) , INTENT(OUT) :: field_out

      !  Local vars

      INTEGER :: i , j
      INTEGER :: julyr, julday, eightday
      REAL :: gmt

      CALL get_julgmt ( date_str , julyr , julday , gmt )
      eightday = ((julday-1) / 8) + 1
!      print *, 'date_str: ', date_str
!      print *, 'julyr, julday: ', julyr, julday
!      print *, 'eightday: ', eightday

      DO j = jts , MIN ( jde-1 , jte )
         DO i = its , MIN (ide-1 , ite )
            field_out(i,j) =  field_in(i,eightday,j)
         END DO
      END DO

   END SUBROUTINE eightday_selector

!---------------------------------------------------------------------

   SUBROUTINE sfcprs (t, q, height, pslv, ter, avgsfct, p, &
                      psfc, ez_method, &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )


      !  Computes the surface pressure using the input height,
      !  temperature and q (already computed from relative
      !  humidity) on p surfaces.  Sea level pressure is used
      !  to extrapolate a first guess.

      IMPLICIT NONE

      REAL, PARAMETER    :: gamma     = 6.5E-3
      REAL, PARAMETER    :: pconst    = 10000.0
      REAL, PARAMETER    :: Rd        = r_d
      REAL, PARAMETER    :: TC        = svpt0 + 17.5

      REAL, PARAMETER    :: gammarg   = gamma * Rd / g
      REAL, PARAMETER    :: rov2      = Rd / 2.

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      LOGICAL , INTENT ( IN ) :: ez_method

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: t, q, height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: pslv ,  ter, avgsfct
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k
      INTEGER , DIMENSION (its:ite,jts:jte) :: k500 , k700 , k850

      LOGICAL                     :: l1
      LOGICAL                     :: l2
      LOGICAL                     :: l3
      LOGICAL                     :: OK

      REAL                        :: gamma78     ( its:ite,jts:jte )
      REAL                        :: gamma57     ( its:ite,jts:jte )
      REAL                        :: ht          ( its:ite,jts:jte )
      REAL                        :: p1          ( its:ite,jts:jte )
      REAL                        :: t1          ( its:ite,jts:jte )
      REAL                        :: t500        ( its:ite,jts:jte )
      REAL                        :: t700        ( its:ite,jts:jte )
      REAL                        :: t850        ( its:ite,jts:jte )
      REAL                        :: tfixed      ( its:ite,jts:jte )
      REAL                        :: tsfc        ( its:ite,jts:jte )
      REAL                        :: tslv        ( its:ite,jts:jte )

      !  We either compute the surface pressure from a time averaged surface temperature
      !  (what we will call the "easy way"), or we try to remove the diurnal impact on the
      !  surface temperature (what we will call the "other way").  Both are essentially
      !  corrections to a sea level pressure with a high-resolution topography field.

      IF ( ez_method ) THEN

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               psfc(i,j) = pslv(i,j) * ( 1.0 + gamma * ter(i,j) / avgsfct(i,j) ) ** ( - g / ( Rd * gamma ) )
            END DO
         END DO

      ELSE

         !  Find the locations of the 850, 700 and 500 mb levels.

         k850 = 0                              ! find k at: P=850
         k700 = 0                              !            P=700
         k500 = 0                              !            P=500

         i = its
         j = jts
         DO k = kts+1 , kte
            IF      (NINT(p(i,k,j)) .EQ. 85000) THEN
               k850(i,j) = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 70000) THEN
               k700(i,j) = k
            ELSE IF (NINT(p(i,k,j)) .EQ. 50000) THEN
               k500(i,j) = k
            END IF
         END DO

         IF ( ( k850(i,j) .EQ. 0 ) .OR. ( k700(i,j) .EQ. 0 ) .OR. ( k500(i,j) .EQ. 0 ) ) THEN

            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  psfc(i,j) = pslv(i,j) * ( 1.0 + gamma * ter(i,j) / t(i,1,j) ) ** ( - g / ( Rd * gamma ) )
               END DO
            END DO

            RETURN
#if 0

            !  Possibly it is just that we have a generalized vertical coord, so we do not
            !  have the values exactly.  Do a simple assignment to a close vertical level.

            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  DO k = kts+1 , kte-1
                     IF ( ( p(i,k,j) - 85000. )  * ( p(i,k+1,j) - 85000. ) .LE. 0.0 ) THEN
                        k850(i,j) = k
                     END IF
                     IF ( ( p(i,k,j) - 70000. )  * ( p(i,k+1,j) - 70000. ) .LE. 0.0 ) THEN
                        k700(i,j) = k
                     END IF
                     IF ( ( p(i,k,j) - 50000. )  * ( p(i,k+1,j) - 50000. ) .LE. 0.0 ) THEN
                        k500(i,j) = k
                     END IF
                  END DO
               END DO
            END DO

            !  If we *still* do not have the k levels, punt.  I mean, we did try.

            OK = .TRUE.
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  IF ( ( k850(i,j) .EQ. 0 ) .OR. ( k700(i,j) .EQ. 0 ) .OR. ( k500(i,j) .EQ. 0 ) ) THEN
                     OK = .FALSE.
                     PRINT '(A)','(i,j) = ',i,j,'  Error in finding p level for 850, 700 or 500 hPa.'
                     DO K = kts+1 , kte
                        PRINT '(A,I3,A,F10.2,A)','K = ',k,'  PRESSURE = ',p(i,k,j),' Pa'
                     END DO
                     PRINT '(A)','Expected 850, 700, and 500 mb values, at least.'
                  END IF
               END DO
            END DO
            IF ( .NOT. OK ) THEN
               CALL wrf_error_fatal ( 'wrong pressure levels' )
            END IF
#endif

         !  We are here if the data is isobaric and we found the levels for 850, 700,
         !  and 500 mb right off the bat.

         ELSE
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
                  k850(i,j) = k850(its,jts)
                  k700(i,j) = k700(its,jts)
                  k500(i,j) = k500(its,jts)
               END DO
            END DO
         END IF

         !  The 850 hPa level of geopotential height is called something special.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               ht(i,j) = height(i,k850(i,j),j)
            END DO
         END DO

         !  The variable ht is now -ter/ht(850 hPa).  The plot thickens.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               ht(i,j) = -ter(i,j) / ht(i,j)
            END DO
         END DO

         !  Make an isothermal assumption to get a first guess at the surface
         !  pressure.  This is to tell us which levels to use for the lapse
         !  rates in a bit.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               psfc(i,j) = pslv(i,j) * (pslv(i,j) / p(i,k850(i,j),j)) ** ht(i,j)
            END DO
         END DO

         !  Get a pressure more than pconst Pa above the surface - p1.  The
         !  p1 is the top of the level that we will use for our lapse rate
         !  computations.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
                  p1(i,j) = 85000.
               ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0. ) THEN
                  p1(i,j) = psfc(i,j) - pconst
               ELSE
                  p1(i,j) = 50000.
               END IF
            END DO
         END DO

         !  Compute virtual temperatures for k850, k700, and k500 layers.  Now
         !  you see why we wanted Q on pressure levels, it all is beginning
         !  to make sense.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               t850(i,j) = t(i,k850(i,j),j) * (1. + 0.608 * q(i,k850(i,j),j))
               t700(i,j) = t(i,k700(i,j),j) * (1. + 0.608 * q(i,k700(i,j),j))
               t500(i,j) = t(i,k500(i,j),j) * (1. + 0.608 * q(i,k500(i,j),j))
            END DO
         END DO

         !  Compute lapse rates between these three levels.  These are
         !  environmental values for each (i,j).

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               gamma78(i,j) = ALOG(t850(i,j) / t700(i,j))  / ALOG (p(i,k850(i,j),j) / p(i,k700(i,j),j) )
               gamma57(i,j) = ALOG(t700(i,j) / t500(i,j))  / ALOG (p(i,k700(i,j),j) / p(i,k500(i,j),j) )
            END DO
         END DO

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               IF      ( ( psfc(i,j) - 95000. ) .GE. 0. ) THEN
                  t1(i,j) = t850(i,j)
               ELSE IF ( ( psfc(i,j) - 85000. ) .GE. 0. ) THEN
                  t1(i,j) = t700(i,j) * (p1(i,j) / (p(i,k700(i,j),j))) ** gamma78(i,j)
               ELSE IF ( ( psfc(i,j) - 70000. ) .GE. 0.) THEN
                  t1(i,j) = t500(i,j) * (p1(i,j) / (p(i,k500(i,j),j))) ** gamma57(i,j)
               ELSE
                  t1(i,j) = t500(i,j)
               ENDIF
            END DO
         END DO

         !  From our temperature way up in the air, we extrapolate down to
         !  the sea level to get a guess at the sea level temperature.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               tslv(i,j) = t1(i,j) * (pslv(i,j) / p1(i,j)) ** gammarg
            END DO
         END DO

         !  The new surface temperature is computed from the with new sea level
         !  temperature, just using the elevation and a lapse rate.  This lapse
         !  rate is -6.5 K/km.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               tsfc(i,j) = tslv(i,j) - gamma * ter(i,j)
            END DO
         END DO

         !  A small correction to the sea-level temperature, in case it is too warm.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               tfixed(i,j) = tc - 0.005 * (tsfc(i,j) - tc) ** 2
            END DO
         END DO

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               l1 = tslv(i,j) .LT. tc
               l2 = tsfc(i,j) .LE. tc
               l3 = .NOT. l1
               IF      ( l2 .AND. l3 ) THEN
                  tslv(i,j) = tc
               ELSE IF ( ( .NOT. l2 ) .AND. l3 ) THEN
                  tslv(i,j) = tfixed(i,j)
               END IF
            END DO
         END DO

         !  Finally, we can get to the surface pressure.

         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
            p1(i,j) = - ter(i,j) * g / ( rov2 * ( tsfc(i,j) + tslv(i,j) ) )
            psfc(i,j) = pslv(i,j) * EXP ( p1(i,j) )
            END DO
         END DO

      END IF

      !  Surface pressure and sea-level pressure are the same at sea level.

!     DO j = jts , MIN(jde-1,jte)
!        DO i = its , MIN(ide-1,ite)
!           IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
!           IF ( ABS ( ter(i,j) )  .LT. 0.1 ) THEN
!              psfc(i,j) = pslv(i,j)
!           END IF
!        END DO
!     END DO

   END SUBROUTINE sfcprs

!---------------------------------------------------------------------

   SUBROUTINE sfcprs2(t, q, height, psfc_in, ter, avgsfct, p, &
                      psfc, ez_method, &
                      ids , ide , jds , jde , kds , kde , &
                      ims , ime , jms , jme , kms , kme , &
                      its , ite , jts , jte , kts , kte )


      !  Computes the surface pressure using the input height,
      !  temperature and q (already computed from relative
      !  humidity) on p surfaces.  Sea level pressure is used
      !  to extrapolate a first guess.

      IMPLICIT NONE

      REAL, PARAMETER    :: Rd        = r_d

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte
      LOGICAL , INTENT ( IN ) :: ez_method

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: t, q, height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: psfc_in ,  ter, avgsfct
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k

      REAL :: tv_sfc_avg , tv_sfc , del_z

      !  Compute the new surface pressure from the old surface pressure, and a
      !  known change in elevation at the surface.

      !  del_z = diff in surface topo, lo-res vs hi-res
      !  psfc = psfc_in * exp ( g del_z / (Rd Tv_sfc ) )


      IF ( ez_method ) THEN
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               tv_sfc_avg = avgsfct(i,j) * (1. + 0.608 * q(i,1,j))
               del_z = height(i,1,j) - ter(i,j)
               psfc(i,j) = psfc_in(i,j) * EXP ( g * del_z / ( Rd * tv_sfc_avg ) )
            END DO
         END DO
      ELSE
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE
               tv_sfc = t(i,1,j) * (1. + 0.608 * q(i,1,j))
               del_z = height(i,1,j) - ter(i,j)
               psfc(i,j) = psfc_in(i,j) * EXP ( g * del_z / ( Rd * tv_sfc     ) )
            END DO
         END DO
      END IF

   END SUBROUTINE sfcprs2

!---------------------------------------------------------------------

   SUBROUTINE sfcprs3( height , p , ter , slp , psfc , &
                       ids , ide , jds , jde , kds , kde , &
                       ims , ime , jms , jme , kms , kme , &
                       its , ite , jts , jte , kts , kte )

      !  Computes the surface pressure by vertically interpolating
      !  linearly (or log) in z the pressure, to the targeted topography.

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , DIMENSION (ims:ime,kms:kme,jms:jme) , INTENT(IN ):: height, p
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(IN ):: ter , slp
      REAL , DIMENSION (ims:ime,        jms:jme) , INTENT(OUT):: psfc

      INTEGER                     :: i
      INTEGER                     :: j
      INTEGER                     :: k

      LOGICAL                     :: found_loc

      REAL :: zl , zu , pl , pu , zm

      !  Loop over each grid point

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            IF ( skip_middle_points_t ( ids , ide , jds , jde , i , j , em_width , hold_ups ) ) CYCLE

            !  Special case where near the ocean level.  Assume that the SLP is a good value.

            IF      ( ter(i,j) .LT. 50 ) THEN
               psfc(i,j) = slp(i,j) + ( p(i,2,j)-p(i,3,j) ) / ( height(i,2,j)-height(i,3,j) ) * ter(i,j)
               CYCLE
            END IF

            !  Find the trapping levels

            found_loc = .FALSE.

            !  Normal sort of scenario - the model topography is somewhere between
            !  the height values of 1000 mb and the top of the model.

            found_k_loc : DO k = kts+1 , kte-2
               IF ( ( height(i,k  ,j) .LE. ter(i,j) ) .AND. &
                    ( height(i,k+1,j) .GT. ter(i,j) ) ) THEN
                  zl = height(i,k  ,j)
                  zu = height(i,k+1,j)
                  zm = ter(i,j)
                  pl = p(i,k  ,j)
                  pu = p(i,k+1,j)
                  psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                  found_loc = .TRUE.
                  EXIT found_k_loc
               END IF
            END DO found_k_loc

            !  Interpolate betwixt slp and the first isobaric level above - this is probably the
            !  usual thing over the ocean.

            IF ( .NOT. found_loc ) THEN
               IF ( slp(i,j) .GE. p(i,2,j) ) THEN
                  zl = 0.
                  zu = height(i,3,j)
                  zm = ter(i,j)
                  pl = slp(i,j)
                  pu = p(i,3,j)
                  psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                  found_loc = .TRUE.
               ELSE
                  found_slp_loc : DO k = kts+1 , kte-3
                     IF ( ( slp(i,j) .GE. p(i,k+1,j) ) .AND. &
                          ( slp(i,j) .LT. p(i,k  ,j) ) ) THEN
                        zl = 0.
                        zu = height(i,k+1,j)
                        zm = ter(i,j)
                        pl = slp(i,j)
                        pu = p(i,k+1,j)
                        psfc(i,j) = EXP ( ( LOG(pl) * ( zm - zu ) + LOG(pu) * ( zl - zm ) ) / ( zl - zu ) )
                        found_loc = .TRUE.
                        EXIT found_slp_loc
                     END IF
                  END DO found_slp_loc
               END IF
            END IF

            !  Did we do what we wanted done.

            IF ( .NOT. found_loc ) THEN
               print *,'i,j = ',i,j
               print *,'p column = ',p(i,2:,j)
               print *,'z column = ',height(i,2:,j)
               print *,'model topo = ',ter(i,j)
               CALL wrf_error_fatal ( ' probs with sfc p computation ' )
            END IF

         END DO
      END DO

   END SUBROUTINE sfcprs3

!---------------------------------------------------------------------

   SUBROUTINE filter_topo ( ht_in , xlat , msftx , &
                            fft_filter_lat , mf_fft , &
                            pos_def , swap_pole_with_next_j , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: fft_filter_lat , mf_fft
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: ht_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx
      LOGICAL :: pos_def , swap_pole_with_next_j

      !  Local vars

      INTEGER :: i , j , j_lat_pos , j_lat_neg , k
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      INTEGER :: i_left , i_right , ii
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: ht_out
      CHARACTER (LEN=256) :: message

      !  The filtering is a simple average on a latitude loop.  Possibly a LONG list of
      !  numbers.  We assume that ALL of the 2d arrays have been transposed so that
      !  each patch has the entire domain size of the i-dim local.

      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal ( 'filtering assumes all values on X' )
      END IF

      !  Starting at the south pole, we find where the
      !  grid distance is big enough, then go back a point.  Continuing to the
      !  north pole, we find the first small grid distance.  These are the
      !  computational latitude loops and the associated computational poles.

      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = MIN(jde-1,jte) , jts , -1
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .GE. fft_filter_lat ) THEN
               j_lat_neg = j
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg

      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos

      !  Set output values to initial input topo values for whole patch.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_out(i,j) = ht_in(i,j)
         END DO
      END DO

      !  Filter the topo at the negative lats.

      DO j = MIN(j_lat_neg,jte) , jts , -1
!        i_kicker = MIN( MAX ( NINT(msftx(its,j)/2) , 1 ) , (ide - ids) / 2 )
         i_kicker = MIN( MAX ( NINT(msftx(its,j)/mf_fft/2) , 1 ) , (ide - ids) / 2 )
         WRITE (message,*) 'SOUTH j = ' , j, ', kicker = ',i_kicker,xlat(its,j),msftx(its,j)
         CALL wrf_debug(10,TRIM(message))
         DO i = its , MIN(ide-1,ite)
            sum = 0.
            DO ik = 1 , i_kicker
               ii = i-ik
               IF ( ii .GE. ids ) THEN
                  i_left = ii
               ELSE
                  i_left = ( ii - ids ) + (ide-1)+1
               END IF
               ii = i+ik
               IF ( ii .LE. ide-1 ) THEN
                  i_right = ii
               ELSE
                  i_right = ( ii - (ide-1) ) + its-1
               END IF
               sum = sum + ht_in(i_left,j) + ht_in(i_right,j)
            END DO
            ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
         END DO
      END DO

      !  Filter the topo at the positive lats.

      DO j = MAX(j_lat_pos,jts) , MIN(jde-1,jte)
!        i_kicker = MIN( MAX ( NINT(msftx(its,j)/2) , 1 ) , (ide - ids) / 2 )
         i_kicker = MIN( MAX ( NINT(msftx(its,j)/mf_fft/2) , 1 ) , (ide - ids) / 2 )
         WRITE (message,*) 'NORTH j = ' , j, ', kicker = ',i_kicker,xlat(its,j),msftx(its,j)
         CALL wrf_debug(10,TRIM(message))
         DO i = its , MIN(ide-1,ite)
            sum = 0.
            DO ik = 1 , i_kicker
               ii = i-ik
               IF ( ii .GE. ids ) THEN
                  i_left = ii
               ELSE
                  i_left = ( ii - ids ) + (ide-1)+1
               END IF
               ii = i+ik
               IF ( ii .LE. ide-1 ) THEN
                  i_right = ii
               ELSE
                  i_right = ( ii - (ide-1) ) + its-1
               END IF
               sum = sum + ht_in(i_left,j) + ht_in(i_right,j)
            END DO
            ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
         END DO
      END DO

      !  Set output values to initial input topo values for whole patch.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_in(i,j) = ht_out(i,j)
         END DO
      END DO

   END SUBROUTINE filter_topo

!---------------------------------------------------------------------
!---------------------------------------------------------------------

   SUBROUTINE filter_topo_old ( ht_in , xlat , msftx , fft_filter_lat , &
                            dummy , &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: fft_filter_lat , dummy
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(INOUT) :: ht_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx


      !  Local vars

      INTEGER :: i , j , j_lat_pos , j_lat_neg
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: ht_out

      !  The filtering is a simple average on a latitude loop.  Possibly a LONG list of
      !  numbers.  We assume that ALL of the 2d arrays have been transposed so that
      !  each patch has the entire domain size of the i-dim local.

      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal ( 'filtering assumes all values on X' )
      END IF

      !  Starting at the south pole, we find where the
      !  grid distance is big enough, then go back a point.  Continuing to the
      !  north pole, we find the first small grid distance.  These are the
      !  computational latitude loops and the associated computational poles.

      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .LT. fft_filter_lat ) THEN
               j_lat_neg = j - 1
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg

      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos

      !  Set output values to initial input topo values for whole patch.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_out(i,j) = ht_in(i,j)
         END DO
      END DO

      !  Filter the topo at the negative lats.

      DO j = j_lat_neg , jts , -1
         i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
         print *,'j = ' , j, ', kicker = ',i_kicker
         DO i = its , MIN(ide-1,ite)
            IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j) + ht_in(i-ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j)
               END DO
               i1 = i - i_kicker + ide -1
               i2 = ide-1
               i3 = ids
               i4 = i-1
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i-ik,j)
               END DO
               i1 = i+1
               i2 = ide-1
               i3 = ids
               i4 = ids + ( i_kicker+i ) - ide
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            END IF
         END DO
      END DO

      !  Filter the topo at the positive lats.

      DO j = j_lat_pos , MIN(jde-1,jte)
         i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
         print *,'j = ' , j, ', kicker = ',i_kicker
         DO i = its , MIN(ide-1,ite)
            IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j) + ht_in(i-ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i+ik,j)
               END DO
               i1 = i - i_kicker + ide -1
               i2 = ide-1
               i3 = ids
               i4 = i-1
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
               sum = 0.0
               DO ik = 1 , i_kicker
                  sum = sum + ht_in(i-ik,j)
               END DO
               i1 = i+1
               i2 = ide-1
               i3 = ids
               i4 = ids + ( i_kicker+i ) - ide
               DO ik = i1 , i2
                  sum = sum + ht_in(ik,j)
               END DO
               DO ik = i3 , i4
                  sum = sum + ht_in(ik,j)
               END DO
               ht_out(i,j) = ( ht_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
            END IF
         END DO
      END DO

      !  Set output values to initial input topo values for whole patch.

      DO j = jts , MIN(jde-1,jte)
         DO i = its , MIN(ide-1,ite)
            ht_in(i,j) = ht_out(i,j)
         END DO
      END DO

   END SUBROUTINE filter_topo_old

!---------------------------------------------------------------------


!+---+-----------------------------------------------------------------+
! Begin addition by Greg Thompson to dry out the stratosphere.
! Starting 3 levels below model top, go downward and search for where
! Theta gradient over three K-levels is less steep than +10 K per 1500 m.
! This threshold approximates a vertical line on a skew-T chart from
! approximately 300 to 240 mb, anything more unstable than this reference
! is probably in the troposphere so pick the K plus 1 point as the
! tropopause and set mixing ratio to a really small values above.
!..Author: Greg Thompson, NCAR-RAL, gthompsn@ucar.edu, 303-497-2805
!..Last modified: 30 Dec 2004
!+---+-----------------------------------------------------------------+

   subroutine dry_stratos ( theta, qv, phb, &
                            ids , ide , jds , jde , kds , kde , &
                            ims , ime , jms , jme , kms , kme , &
                            its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN)        :: ids , ide , jds , jde , kds , kde , &
                                     ims , ime , jms , jme , kms , kme , &
                                     its , ite , jts , jte , kts , kte

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(IN)     :: theta, phb
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT)  :: qv

      !  Local vars

      INTEGER :: i, j, k, kk, istart, iend, jstart, jend, kstart, kend
      REAL    :: ht1, ht2, theta1, theta2, htz, sat85, p_std_atmos
      CHARACTER*256:: str_debug
      ! Saturation vapor pressure at T = -85C.
      DATA sat85 /0.0235755574/

      do i = 1, 256
         str_debug(i:i) = char(0)
      enddo

      istart = its
      iend   = MIN(ide-1,ite)
      jstart = jts
      jend   = MIN(jde-1,jte)
      kstart = kts
      kend   = kte-1
      DO j = jstart, jend
         DO i = istart, iend
            DO k = kend-3, kstart, -1
               ht1 = phb(i,k,j)/9.8
               ht2 = phb(i,k+2,j)/9.8
               theta1 = theta(i,k,j)
               theta2 = theta(i,k+2,j)
               if ( (((theta2-theta1)/(ht2-ht1)) .lt. 10./1500. ) .AND. (ht1.gt.4000.) ) then
                  DO kk = k+3, kend
                     htz = phb(i,kk,j)/9.8
                     p_std_atmos = exp(log(1.0-htz/44307.692)/0.19)*101325.0
                     qv(i,kk,j) = 0.622*sat85/(p_std_atmos-sat85)
                  END DO
                  goto 79
               end if
            END DO
 79         continue
         END DO
      END DO

   END SUBROUTINE dry_stratos

!+---+-----------------------------------------------------------------+
!..Hardwire snow cover above a pre-specified altitude.
!.. Starting altitude for snow (snow_startz) depends on latitude
!.. and is 3900 m at 35-deg lowering to 250km (linearly) by 65-deg lat.
!.. Alter WEASD linear function from 0 at snow_startz to 999 mm at 4 km.
!.. Author: Greg Thompson, NCAR-RAL, gthompsn@ucar.edu, 303-497-2805
!.. Last modified: 27 Dec 2008
!+---+-----------------------------------------------------------------+

      real function snowHires (snow_in, latitude, elev, date_str, i,j)
      IMPLICIT NONE

      REAL, INTENT(IN):: latitude, elev, snow_in
      INTEGER, INTENT(IN):: i, j
      CHARACTER (LEN=24), INTENT(IN) :: date_str

      REAL :: snow_startz, del_lat, season_factor, snow_out
      REAL :: gmt
      INTEGER :: day_peak, day_of_year, julyr
      CHARACTER (LEN=256) :: dbg_msg

      CALL get_julgmt ( date_str , julyr , day_of_year , gmt )

      if (latitude .gt. 0.0) then
         del_lat = (65.-latitude)/(65.-35.)
         day_peak = 80
      else
         del_lat = (-65.-latitude)/(-65.+35.)
         day_peak = 264
      endif

      snow_startz = (3900.-250.)*del_lat + 250.
      snow_startz = max(250., min(3900., snow_startz))

      season_factor = 1.
      snow_out = 0.
      IF (elev .GT. snow_startz) THEN
         season_factor = ABS(COS((day_of_year - day_peak)*0.5*0.0174533))
         snow_out = 0.999*(elev-snow_startz)/(4000.-snow_startz)
         write(dbg_msg,*) 'DEBUG_GT_SNOW ', day_of_year, latitude, elev, snow_in, snow_startz, season_factor, snow_out,i, j
         CALL wrf_debug (150, dbg_msg)
      ENDIF

      snowHires = MAX(snow_in, season_factor * snow_out)

      END FUNCTION snowHires

!+---+-----------------------------------------------------------------+
!+---+-----------------------------------------------------------------+

      real function make_IceNumber (Q_ice, temp)

      IMPLICIT NONE
      REAL, PARAMETER:: Ice_density = 890.0
      REAL, PARAMETER:: PI = 3.1415926536
      integer idx_rei
      real corr, reice, deice, Q_ice, temp
      double precision lambda

!+---+-----------------------------------------------------------------+
!..Table of lookup values of radiative effective radius of ice crystals
!.. as a function of Temperature from -94C to 0C.  Taken from WRF RRTMG
!.. radiation code where it is attributed to Jon Egill Kristjansson
!.. and coauthors.
!+---+-----------------------------------------------------------------+

      real retab(95)
      data retab /                                                      &
         5.92779, 6.26422, 6.61973, 6.99539, 7.39234,                   &
         7.81177, 8.25496, 8.72323, 9.21800, 9.74075, 10.2930,          &
         10.8765, 11.4929, 12.1440, 12.8317, 13.5581, 14.2319,          &
         15.0351, 15.8799, 16.7674, 17.6986, 18.6744, 19.6955,          &
         20.7623, 21.8757, 23.0364, 24.2452, 25.5034, 26.8125,          &
         27.7895, 28.6450, 29.4167, 30.1088, 30.7306, 31.2943,          &
         31.8151, 32.3077, 32.7870, 33.2657, 33.7540, 34.2601,          &
         34.7892, 35.3442, 35.9255, 36.5316, 37.1602, 37.8078,          &
         38.4720, 39.1508, 39.8442, 40.5552, 41.2912, 42.0635,          &
         42.8876, 43.7863, 44.7853, 45.9170, 47.2165, 48.7221,          &
         50.4710, 52.4980, 54.8315, 57.4898, 60.4785, 63.7898,          &
         65.5604, 71.2885, 75.4113, 79.7368, 84.2351, 88.8833,          &
         93.6658, 98.5739, 103.603, 108.752, 114.025, 119.424,          &
         124.954, 130.630, 136.457, 142.446, 148.608, 154.956,          &
         161.503, 168.262, 175.248, 182.473, 189.952, 197.699,          &
         205.728, 214.055, 222.694, 231.661, 240.971, 250.639/

!+---+-----------------------------------------------------------------+
!..From the model 3D temperature field, subtract 179K for which
!.. index value of retab as a start.  Value of corr is for
!.. interpolating between neighboring values in the table.
!+---+-----------------------------------------------------------------+

      idx_rei = int(temp-179.)
      idx_rei = min(max(idx_rei,1),94)
      corr = temp - int(temp)
      reice = retab(idx_rei)*(1.-corr) + retab(idx_rei+1)*corr
      deice = 2.*reice * 1.E-6

!+---+-----------------------------------------------------------------+
!..Now we have the final radiative effective size of ice (as function
!.. of temperature only).  This size represents 3rd moment divided by
!.. second moment of the ice size distribution, so we can compute a
!.. number concentration from the mean size and mass mixing ratio.
!.. The mean (radiative effective) diameter is 3./Slope for an inverse
!.. exponential size distribution.  So, starting with slope, work
!.. backwords to get number concentration.
!+---+-----------------------------------------------------------------+

      lambda = 3.0 / deice
      make_IceNumber = Q_ice * lambda*lambda*lambda / (PI*Ice_density)

!+---+-----------------------------------------------------------------+
!..Example1: Common ice size coming from Thompson scheme is about 30 microns.
!.. An example ice mixing ratio could be 0.001 g/kg for a temperature of -50C.
!.. Remember to convert both into MKS units.  This gives N_ice=357652 per kg.
!..Example2: Lower in atmosphere at T=-10C matching ~162 microns in retab,
!.. and assuming we have 0.1 g/kg mixing ratio, then N_ice=28122 per kg,
!.. which is 28 crystals per liter of air if the air density is 1.0.
!+---+-----------------------------------------------------------------+

      return
      end function make_IceNumber

!+---+-----------------------------------------------------------------+
!+---+-----------------------------------------------------------------+

      real function make_DropletNumber (Q_cloud, qnwfa, xland)

      IMPLICIT NONE

      real:: Q_cloud, qnwfa, xland

      real, parameter:: PI = 3.1415926536
      real, parameter:: am_r = PI*1000./6.
      real, dimension(15), parameter:: g_ratio = (/24,60,120,210,336,   &
     &                504,720,990,1320,1716,2184,2730,3360,4080,4896/)
      double precision:: lambda, qnc
      real:: q_nwfa, x1, xDc
      integer:: nu_c

!+---+

      if (qnwfa .le. 0.0) then

         if ((xland-1.5).gt.0.) then                                     !--- Ocean
            xDc = 17.E-6
            nu_c = 12
         else                                                            !--- Land
            xDc = 11.E-6
            nu_c = 4
         endif

      else
         q_nwfa = MAX(99.E6, MIN(qnwfa,5.E10))
         nu_c = MAX(2, MIN(NINT(2.5E10/q_nwfa), 15))

         x1 = MAX(1., MIN(q_nwfa*1.E-9, 10.)) - 1.
         xDc = (30. - x1*20./9.) * 1.E-6
      endif

      lambda = (4.0D0 + nu_c) / xDc
      qnc = Q_cloud / g_ratio(nu_c) * lambda*lambda*lambda / am_r
      make_DropletNumber = SNGL(qnc)

      return
      end function make_DropletNumber

!+---+-----------------------------------------------------------------+
!+---+-----------------------------------------------------------------+

      real function make_RainNumber (Q_rain, temp)

      IMPLICIT NONE

      real, intent(in):: Q_rain, temp
      double precision:: lambda, N0, qnr
      real, parameter:: PI = 3.1415926536
      real, parameter:: am_r = PI*1000./6.

      !+---+-----------------------------------------------------------------+ 
      !.. Not thrilled with it, but set Y-intercept parameter to Marshal-Palmer value
      !.. that basically assumes melting snow becomes typical rain. However, for
      !.. -2C < T < 0C, make linear increase in exponent to attempt to keep
      !.. supercooled collision-coalescence (warm-rain) similar to drizzle rather
      !.. than bigger rain drops.  While this could also exist at T>0C, it is
      !.. more difficult to assume it directly from having mass and not number.
      !+---+-----------------------------------------------------------------+ 

      N0 = 8.E6

      if (temp .le. 271.15) then
         N0 = 8.E8      
      elseif (temp .gt. 271.15 .and. temp.lt.273.15) then
         N0 = 8. * 10**(279.15-temp)
      endif

      lambda = SQRT(SQRT(N0*am_r*6.0/Q_rain))
      qnr = Q_rain / 6.0 * lambda*lambda*lambda / am_r
      make_RainNumber = SNGL(qnr)

      return
      end function make_RainNumber

!+---+-----------------------------------------------------------------+
!+---+-----------------------------------------------------------------+


   SUBROUTINE init_module_initialize
   END SUBROUTINE init_module_initialize

!---------------------------------------------------------------------

END MODULE module_initialize_real
#endif
