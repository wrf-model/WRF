!-------------------------------------------------------------------

   SUBROUTINE start_domain_em ( grid, allowed_to_read &
! Actual arguments generated from Registry
# include "dummy_new_args.inc"
!
)

   USE module_domain, ONLY : domain, wrfu_timeinterval, get_ijk_from_grid, &
        domain_setgmtetc
   USE module_state_description
   USE module_driver_constants
   USE module_wrf_error
   USE module_model_constants
   USE module_bc, ONLY : boundary_condition_check, set_physical_bc2d, set_physical_bc3d, bdyzone
   USE module_bc_em, ONLY: lbc_fcx_gcx, set_w_surface
   USE module_configure, ONLY : model_to_grid_config_rec, model_config_rec, grid_config_rec_type
   USE module_tiles, ONLY : set_tiles
#ifdef DM_PARALLEL
   USE module_dm, ONLY : wrf_dm_min_real, wrf_dm_max_real, wrf_dm_maxval, &
        ntasks_x, ntasks_y, &
        local_communicator_periodic, local_communicator, mytask, ntasks
#else
   USE module_dm, ONLY : wrf_dm_min_real, wrf_dm_max_real
#endif
   USE module_comm_dm
   USE module_llxy, ONLY : proj_cassini
   USE module_physics_init
   USE module_sf_noahmpdrv, ONLY : groundwater_init
   USE module_lightning_driver, ONLY : lightning_init
   USE module_fr_fire_driver_wrf, ONLY : fire_driver_em_init
   USE module_stoch, ONLY : setup_rand_perturb, rand_seed, update_stoch, initialize_stoch
   USE module_trajectory, ONLY : trajectory_init
#if (WRF_CHEM == 1)
   USE module_aerosols_sorgam, ONLY: sum_pm_sorgam
   USE module_gocart_aerosols, ONLY: sum_pm_gocart
   USE module_mosaic_driver, ONLY: sum_pm_mosaic
   USE module_input_tracer, ONLY: initialize_tracer
   USE module_aerosols_soa_vbs, only: sum_pm_soa_vbs
#endif
   USE module_diag_pld, ONLY : pld
   USE module_diag_zld, ONLY : zld
   USE module_trad_fields, ONLY : trad_fields

!!debug
!USE module_compute_geop

   USE module_model_constants
   USE module_avgflx_em, ONLY : zero_avgflx

   IMPLICIT NONE
   !  Input data.
   TYPE (domain)          :: grid

   LOGICAL , INTENT(IN)   :: allowed_to_read

   !  Definitions of dummy arguments to this routine (generated from Registry).
# include "dummy_new_decl.inc"

   !  Structure that contains run-time configuration (namelist) data for domain
   TYPE (grid_config_rec_type)              :: config_flags

   !  Local data
   INTEGER                             ::                       &
                                  ids, ide, jds, jde, kds, kde, &
                                  ims, ime, jms, jme, kms, kme, &
                                  ips, ipe, jps, jpe, kps, kpe, &
                                  its, ite, jts, jte, kts, kte, &
                                  ij,i,j,k,ii,jj,kk,loop,error,l

   INTEGER ::              imsx, imex, jmsx, jmex, kmsx, kmex,    &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,    &
                           imsy, imey, jmsy, jmey, kmsy, kmey,    &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey

   INTEGER     :: i_m

   REAL        :: p_top_test, p00, t00, a, tiso, p_surf, pd_surf, temp, tiso_tmp
   REAL        :: p_strat, a_strat
#if (WRF_CHEM == 1)
   REAL        RGASUNIV ! universal gas constant [ J/mol-K ]
   PARAMETER ( RGASUNIV = 8.314510 )
      REAL,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: &
                      z_at_w,convfac
   REAL :: tempfac
#endif

   REAL :: qvf1, qvf2, qvf
   REAL :: pfu, pfd, phm
   REAL :: MPDT
   REAL :: spongeweight
   LOGICAL :: first_trip_for_this_domain, start_of_simulation, fill_w_flag
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

#if (WRF_CHEM != 1)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: cldfra_old
#endif

   REAL :: lat1 , lat2 , lat3 , lat4
   REAL :: lon1 , lon2 , lon3 , lon4
   INTEGER :: num_points_lat_lon , iloc , jloc
   CHARACTER (LEN=256) :: message, a_message
   TYPE(WRFU_TimeInterval) :: stepTime
   REAL, DIMENSION(:,:), ALLOCATABLE :: clat_glob
   logical :: f_flux  ! flag for computing averaged fluxes in cu_gd

   INTEGER :: idex, jdex
   INTEGER :: im1,ip1,jm1,jp1
   REAL :: hx,hy,pi

   REAL :: w_max, w_min
   LOGICAL :: w_needs_to_be_set
!  
! Define variables local (topo_wind local vars)
   REAL    :: alpha, vfac
   INTEGER :: j_save

   !  For a global domain

   INTEGER :: alloc_status
   CHARACTER (LEN=256) :: alloc_err_message

!..Need to fill special height var for setting up initial condition.  G. Thompson
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: z_at_q
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: dz8w   !mchen

!  CCN for MP=18 initializatio
   REAL :: ccn_max_val

   REAL :: dt_s, dx_km

   REAL :: max_mf, max_rot_angle
   CALL get_ijk_from_grid ( grid ,                              &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           ips, ipe, jps, jpe, kps, kpe,        &
                           imsx, imex, jmsx, jmex, kmsx, kmex,  &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                           imsy, imey, jmsy, jmey, kmsy, kmey,  &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey )

   kts = kps ; kte = kpe     ! note that tile is entire patch
   its = ips ; ite = ipe     ! note that tile is entire patch
   jts = jps ; jte = jpe    ! note that tile is entire patch
#if (WRF_CHEM != 1)
         ALLOCATE(CLDFRA_OLD(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_OLD = 0.
#endif
   ALLOCATE(dz8w(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; dz8w = 0.
   ALLOCATE(z_at_q(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; z_at_q = 0.
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   IF ( ( MOD (ide-ids,config_flags%parent_grid_ratio) .NE. 0 ) .OR. &
        ( MOD (jde-jds,config_flags%parent_grid_ratio) .NE. 0 ) ) THEN
      WRITE(message, FMT='(A,I2,":  Both MOD(",I4,"-",I1,",",I2,") and MOD(",I4,"-",I1,",",I2,") must = 0" )') &
         "Nested dimensions are illegal for domain ",grid%id,ide,ids,config_flags%parent_grid_ratio,&
         jde,jds,config_flags%parent_grid_ratio
      CALL wrf_error_fatal ( message )
   END IF

   IF ( config_flags%polar ) THEN
!write(0,*)"<stdin>",__LINE__,' clat ',ips,ipe,jps,jpe
!do j = jps,jpe
!write(0,*)"<stdin>",__LINE__,' clat ',ids,j,grid%clat(ips,j)
!enddo

#ifdef DM_PARALLEL
! WARNING:  this might present scaling issues on very large numbers of processors
     alloc_err_message = ' '
     alloc_err_message(1:12) = 'NO PROBLEMOS'
#if 0
     ALLOCATE( clat_glob(ids:ide,jds:jde), STAT=alloc_status, ERRMSG=alloc_err_message )
#else
     ALLOCATE( clat_glob(ids:ide,jds:jde), STAT=alloc_status)
     alloc_err_message = 'Allocation of space for a global field failed.'
#endif

     IF ( alloc_status .NE. 0 ) THEN
        CALL wrf_message ( TRIM(alloc_err_message) )
        CALL wrf_error_fatal ( 'Error allocating entire domain size of 2d array CLAT for global domain' )
     END IF

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
#endif
   ENDIF

! here we check to see if the boundary conditions are set properly

   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )
! make sure that topo_wind option has var_sso data available
    IF ((config_flags%topo_wind .EQ. 1) .AND. (.NOT. grid%got_var_sso)) THEN
      CALL wrf_error_fatal ("topo_wind requires VAR_SSO data")
    ENDIF

!kludge - need to stop CG from resetting precip and phys tendencies to zero
!         when we are in here due to a nest being spawned, we want to still
!         recompute the base state, but that is about it
   !  This is temporary and will need to be changed when grid%itimestep is removed.

   IF ( grid%itimestep .EQ. 0 ) THEN
      first_trip_for_this_domain = .TRUE.
   ELSE
      first_trip_for_this_domain = .FALSE.
   END IF

   IF ( .not. ( config_flags%restart .or. grid%moved ) ) THEN
       grid%itimestep=0
   ENDIF

   IF ( config_flags%restart .or. grid%moved .or. config_flags%cycling) THEN
       first_trip_for_this_domain = .TRUE.
   ENDIF

   ! Print out the maximum map scale factor on the coarse domain

   IF ( ( first_trip_for_this_domain ) .AND. ( grid%id .EQ. 1 ) .AND. &
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

!-----------------------------------------------------------------------
! Do a gross check on the time step for the most coarse grid (not for
! adaptive time steps. If the value is significantly larger than reasonable,
! then stop immediately. Likely the user made a mistake.
! If the rule of thumb is 6 DX = DT, then anything > 10 is flagged.
! Only look at domain 1, only do this for the firt time into this routine,
! only look at real-data cases (where the map projection is not Cartesian), 
! and only look at user-specified time steps (not the adaptive dt option).
!-----------------------------------------------------------------------
   IF ( ( grid%id .EQ. 1 ) .AND. &
        ( first_trip_for_this_domain ) .AND. &
        ( config_flags%map_proj .NE. 0 ) .AND. &
        ( .NOT. config_flags%use_adaptive_time_step ) ) THEN
      dt_s = REAL(config_flags%time_step) + &
             REAL(config_flags%time_step_fract_num) / &
             REAL(config_flags%time_step_fract_den)
      dx_km = MIN ( config_flags%dx , config_flags%dy ) / 1000.
      WRITE (message,*) 'D01: Time step                              = ',dt_s ,' (s)'
      CALL wrf_message ( TRIM(message) ) 
      WRITE (message,*) 'D01: Grid Distance                          = ',dx_km ,' (km)'
      CALL wrf_message ( TRIM(message) ) 
      WRITE (message,*) 'D01: Grid Distance Ratio dt/dx              = ', dt_s / dx_km, ' (s/km)'
      CALL wrf_message ( TRIM(message) ) 
      WRITE (message,*) 'D01: Ratio Including Maximum Map Factor     = ', dt_s / (dx_km / max_mf) , ' (s/km)'
      CALL wrf_message ( TRIM(message) ) 
      WRITE (message,*) 'D01: NML defined reasonable_time_step_ratio = ', config_flags%reasonable_time_step_ratio
      CALL wrf_message ( TRIM(message) ) 
      IF ( dt_s / dx_km > config_flags%reasonable_time_step_ratio ) THEN
         CALL wrf_message ( 'The time step is probably too large for this grid distance, reduce it.' )
         WRITE (message, * ) 'If you are sure of your settings, set reasonable_time_step_ratio in namelist.input > ' &
                             ,dt_s / (dx_km / max_mf)
         CALL wrf_message ( TRIM(message) ) 
         CALL wrf_error_fatal ( '--- ERROR: Time step too large')
      END IF
   END IF

    if(config_flags%cycling) then
! Clear the buckets for diagnostics at initial time
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
            grid%prec_acc_nc(i,j) = 0.
            grid%snow_acc_nc(i,j)  = 0.
            grid%sfcrunoff  (i,j) = 0.
            grid%udrunoff   (i,j) = 0.
! acsnow, and acrunoff are run-total
            grid%acrunoff   (i,j) = 0.
            grid%acsnow     (i,j) = 0.
       ENDDO
       ENDDO
    endif

!   --- SETUP AND INITIALIZE STOCHASTIC PERTURBATION SCHEMES ---

   IF ( grid%itimestep .EQ. 0 ) THEN
      first_trip_for_this_domain = .TRUE.
   ELSE
      first_trip_for_this_domain = .FALSE.
   END IF

   IF ( .not. ( config_flags%restart .or. grid%moved .or. config_flags%hrrr_cycling) ) THEN
       grid%itimestep=0
   ENDIF

   IF ( config_flags%restart .or. grid%moved .or. config_flags%hrrr_cycling) THEN
       first_trip_for_this_domain = .TRUE.
   ENDIF

   CALL INITIALIZE_STOCH  (grid, config_flags,                  &    
                           first_trip_for_this_domain,          &    
                           ips, ipe, jps, jpe, kps, kpe,        &    
                           ids, ide, jds, jde, kds, kde,        &    
                           ims, ime, jms, jme, kms, kme,        &    
                           its, ite, jts, jte, kts, kte,        &    
                           imsx, imex, jmsx, jmex, kmsx, kmex,  &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                           imsy, imey, jmsy, jmey, kmsy, kmey,  &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey   )

!   --- END SETUP STOCHASTIC PERTURBATION SCHEMES ----------



! wig: Add a combined exponential+linear weight on the mother boundaries
!      following code changes by Ruby Leung. For the nested grid, there
!      appears to be some problems when a sponge is used. The points where
!      processors meet have problematic values.

   CALL lbc_fcx_gcx ( grid%fcx , grid%gcx , grid%spec_bdy_width , &
                      grid%spec_zone , grid%relax_zone , grid%dt , config_flags%spec_exp , &
                      config_flags%specified , config_flags%nested )

   IF ( config_flags%nested ) THEN
     grid%dtbc = 0.
   ENDIF

   IF ( ( grid%id .NE. 1 ) .AND. ( .NOT. config_flags%input_from_file ) ) THEN

      !  Every time a domain starts or every time a domain moves, this routine is called.  We want
      !  the center (middle) lat/lon of the grid for the metacode.  The lat/lon values are
      !  defined at mass points.  Depending on the even/odd points in the SN and WE directions,
      !  we end up with the middle point as either 1 point or an average of either 2 or 4 points.
      !  Add to this, the need to make sure that we are on the correct patch to retrieve the
      !  value of the lat/lon, AND that the lat/lons (for an average) may not all be on the same
      !  patch.  Once we find the correct value for lat lon, we need to keep it around on all patches,
      !  which is where the wrf_dm_min_real calls come in.
      !  If this is the most coarse domain, we do not go in here.  Also, if there is an input file
      !  (which has the right values for the middle lat/lon) we do not go in this IF test.

      IF      ( ( MOD(ide,2) .EQ. 0 ) .AND. ( MOD(jde,2) .EQ. 0 ) ) THEN
         num_points_lat_lon = 1
         iloc = ide/2
         jloc = jde/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )
         CALL nl_set_cen_lat ( grid%id , lat1 )
         CALL nl_set_cen_lon ( grid%id , lon1 )
      ELSE IF ( ( MOD(ide,2) .NE. 0 ) .AND. ( MOD(jde,2) .EQ. 0 ) ) THEN
         num_points_lat_lon = 2
         iloc = (ide-1)/2
         jloc =  jde   /2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )

         iloc = (ide+1)/2
         jloc =  jde   /2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = grid%xlat (iloc,jloc)
            lon2 = grid%xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )

         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 ) * 0.50 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 ) * 0.50 )
      ELSE IF ( ( MOD(ide,2) .EQ. 0 ) .AND. ( MOD(jde,2) .NE. 0 ) ) THEN
         num_points_lat_lon = 2
         iloc =  ide   /2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )

         iloc =  ide   /2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = grid%xlat (iloc,jloc)
            lon2 = grid%xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )

         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 ) * 0.50 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 ) * 0.50 )
      ELSE IF ( ( MOD(ide,2) .NE. 0 ) .AND. ( MOD(jde,2) .NE. 0 ) ) THEN
         num_points_lat_lon = 4
         iloc = (ide-1)/2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat1 = grid%xlat (iloc,jloc)
            lon1 = grid%xlong(iloc,jloc)
         ELSE
            lat1 = 99999.
            lon1 = 99999.
         END IF
         lat1 = wrf_dm_min_real ( lat1 )
         lon1 = wrf_dm_min_real ( lon1 )

         iloc = (ide+1)/2
         jloc = (jde-1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat2 = grid%xlat (iloc,jloc)
            lon2 = grid%xlong(iloc,jloc)
         ELSE
            lat2 = 99999.
            lon2 = 99999.
         END IF
         lat2 = wrf_dm_min_real ( lat2 )
         lon2 = wrf_dm_min_real ( lon2 )

         iloc = (ide-1)/2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat3 = grid%xlat (iloc,jloc)
            lon3 = grid%xlong(iloc,jloc)
         ELSE
            lat3 = 99999.
            lon3 = 99999.
         END IF
         lat3 = wrf_dm_min_real ( lat3 )
         lon3 = wrf_dm_min_real ( lon3 )

         iloc = (ide+1)/2
         jloc = (jde+1)/2
         IF ( ( ips .LE. iloc ) .AND. ( ipe .GE. iloc ) .AND. &
              ( jps .LE. jloc ) .AND. ( jpe .GE. jloc ) ) THEN
            lat4 = grid%xlat (iloc,jloc)
            lon4 = grid%xlong(iloc,jloc)
         ELSE
            lat4 = 99999.
            lon4 = 99999.
         END IF
         lat4 = wrf_dm_min_real ( lat4 )
         lon4 = wrf_dm_min_real ( lon4 )

         CALL nl_set_cen_lat ( grid%id , ( lat1 + lat2 + lat3 + lat4 ) * 0.25 )
         CALL nl_set_cen_lon ( grid%id , ( lon1 + lon2 + lon3 + lon4 ) * 0.25 )
      END IF
   END IF

   !  Make sure that the base-state vales have not changed between what is in the input file
   !  and the namelist file.

   IF ( .NOT. grid%this_is_an_ideal_run ) THEN
      CALL nl_get_p_top_requested  ( 1 , p_top_test )
      IF ( grid%p_top .NE. p_top_test ) THEN
         CALL wrf_error_fatal ( 'start_em: p_top from the namelist does not match p_top from the input file.' )
      END IF
   END IF

   IF ( config_flags%use_baseparam_fr_nml ) then
      CALL nl_get_base_pres        ( 1 , p00        )
      CALL nl_get_base_temp        ( 1 , t00        )
      CALL nl_get_base_lapse       ( 1 , a          )
      CALL nl_get_iso_temp         ( 1 , tiso       )
      CALL nl_get_base_lapse_strat ( 1 , a_strat    )
      CALL nl_get_base_pres_strat  ( 1 , p_strat    )
      IF ( ( t00 .LT. 100. .or. p00 .LT. 10000.) .AND. ( .NOT. grid%this_is_an_ideal_run ) ) THEN
         WRITE(wrf_err_message,*) 'start_em: BAD BASE STATE for T00 or P00 in namelist.input file'
         CALL wrf_error_fatal(TRIM(wrf_err_message))
      END IF

   ELSE
   ! get these constants from model data

      t00     = grid%t00
      p00     = grid%p00
      a       = grid%tlp
      tiso    = grid%tiso
      a_strat = grid%tlp_strat
      p_strat = grid%p_strat
      IF ( ( t00 .LT. 100. .or. p00 .LT. 10000.) .AND. ( .NOT. grid%this_is_an_ideal_run ) ) THEN
         WRITE(wrf_err_message,*)&
         'start_em: did not find base state parameters in wrfinput. Add use_baseparam_fr_nml = .t. in &dynamics and rerun'
         CALL wrf_error_fatal(TRIM(wrf_err_message))
      ENDIF

   ENDIF

!  check if tiso in the input file agrees with namelist value

   CALL nl_get_iso_temp   ( 1 , tiso_tmp )
   IF ( ( tiso_tmp .NE. tiso ) .AND. ( .NOT. grid%this_is_an_ideal_run ) ) THEN
      WRITE(wrf_err_message,*)&
      'start_em: namelist iso_temp is not equal to iso_temp in wrfinput. Reset nml value and rerun'
      CALL wrf_error_fatal(TRIM(wrf_err_message))
   ENDIF

   IF ( .NOT. config_flags%restart .AND. &
        (( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ))) THEN

      IF ( config_flags%map_proj .EQ. 0 ) THEN
         CALL wrf_error_fatal ( 'start_domain: Idealized case cannot have a separate nested input file' )
      END IF

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

      IF ( grid%c1f(1) .NE. 1. ) THEN
         CALL wrf_debug ( 0 , '---- WARNING : Maybe old non-HVC input, setting default 1d array values for TF' )
         IF ( grid%hybrid_opt .NE. 0 ) THEN
            CALL wrf_error_fatal ( '---- Error : Cannot use old input and try to use hybrid vertical coordinate option' )
         END IF
         DO k = 1, kte
            grid%c1f(k) = 1.
            grid%c2f(k) = 0.
            grid%c3f(k) = grid%znw(k)
            grid%c4f(k) = 0.
            grid%c1h(k) = 1.
            grid%c2h(k) = 0.
            grid%c3h(k) = grid%znu(k)
            grid%c4h(k) = 0.
         END DO
      END IF

      !  With newer versions of WRF, the grid%t_2 field might come in as "zero"
      !  from an older data set. Yikes, but it can be corrected unless the user
      !  has requested moist theta.

      IF ( grid%t_2(its,kte-1,jts) .EQ. 0. ) THEN
         CALL wrf_debug ( 0 , '---- WARNING : Older v3 input data detected' )
         IF ( grid%use_theta_m .NE. 0 ) THEN
            CALL wrf_error_fatal ( '---- Error : Cannot use moist theta option with old data' )
         END IF
         DO j = jts, MIN(jte,jde-1)
            DO k = 1, kte-1
               DO i = its, MIN(ite,ide-1)
                  grid%t_2(i,k,j) = grid%th_phy_m_t0(i,k,j)
               END DO
            END DO
         END DO
      END IF

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            !  Base state pressure is a function of eta level and terrain, only, plus
            !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
            !  temperature, K), A (temperature difference, from 1000 mb to 300 mb, K),
            !  tiso (isothermal temperature at tropopause/lower stratosphere),
            !  p_strat (pressure at top of isothermal layer), A_strat (lapse rate in
            !  stratosphere above isothermal layer)
            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
            DO k = 1, kte-1
               grid%pb(i,k,j) = grid%c3h(k)*(p_surf - grid%p_top) + grid%c4h(k) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
               IF ( grid%pb(i,k,j) .LT. p_strat ) THEN
                  temp = tiso + A_strat * LOG ( grid%pb(i,k,j)/p_strat )
               ENDIF
               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO
            !  Base state mu is defined as base state surface pressure minus grid%p_top
            grid%MUB(i,j) = p_surf - grid%p_top
            !  Integrate base geopotential, starting at terrain elevation.  This assures that
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.
            grid%phb(i,1,j) = grid%ht(i,j) * g
            IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
               DO kk  = 2,kte
                  k = kk - 1
                  grid%phb(i,kk,j) = grid%phb(i,kk-1,j) - grid%dnw(kk-1)*(grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))*grid%alb(i,kk-1,j)
               END DO
            ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
               DO k  = 2,kte
                  pfu = grid%c3f(k  )*grid%MUB(i,j) + grid%c4f(k  ) + grid%p_top
                  pfd = grid%c3f(k-1)*grid%MUB(i,j) + grid%c4f(k-1) + grid%p_top
                  phm = grid%c3h(k-1)*grid%MUB(i,j) + grid%c4h(k-1) + grid%p_top
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO
            END IF
         END DO
      END DO
   ENDIF
   IF(.not.config_flags%restart)THEN
!  if this is for a nested domain, the defined/interpolated fields are the _2
     IF ( first_trip_for_this_domain ) THEN
! data that is expected to be zero must be explicitly initialized as such
!    grid%h_diabatic = 0.
       DO j = jts,min(jte,jde-1)
       DO k = kts,kte-1
       DO i = its, min(ite,ide-1)
           IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
             grid%t_1(i,k,j)=grid%t_2(i,k,j)
           ENDIF
       ENDDO
       ENDDO
       ENDDO
       DO j = jts,min(jte,jde-1)
       DO k = kts,kte
       DO i = its, min(ite,ide-1)
          grid%ph_1(i,k,j)=grid%ph_2(i,k,j)
       ENDDO
       ENDDO
       ENDDO
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
           IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
             grid%MU_1(i,j)=grid%MU_2(i,j)
           ENDIF
       ENDDO
       ENDDO
     END IF
!  reconstitute base-state fields
    IF(config_flags%max_dom .EQ. 1)THEN
! with single domain, grid%t_init from wrfinput is OK to use
     DO j = jts,min(jte,jde-1)
     DO k = kts,kte-1
     DO i = its, min(ite,ide-1)
       IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
         grid%pb(i,k,j) = grid%c3h(k  )*grid%MUB(i,j) + grid%c4h(k  ) + grid%p_top
         grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
       ENDIF
     ENDDO
     ENDDO
     ENDDO
    ELSE ! there is more than 1 domain
     IF ( .NOT. grid%this_is_an_ideal_run ) THEN ! this is a real run
        DO j = jts,min(jte,jde-1)
        DO k = kts,kte-1
        DO i = its, min(ite,ide-1)
          IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
            grid%pb(i,k,j) = grid%c3h(k  )*grid%MUB(i,j) + grid%c4h(k  ) + grid%p_top
            temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
            IF ( grid%pb(i,k,j) .LT. p_strat ) THEN
               temp = tiso + A_strat * LOG ( grid%pb(i,k,j)/p_strat )
            ENDIF
            grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
            grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
       ENDIF
      ENDDO
      ENDDO
      ENDDO
      IF ( ( config_flags%rebalance .EQ. 1 ) .OR. &
           ( ( config_flags%rebalance .EQ. 2 ) .AND. ( config_flags%vert_refine_method .EQ. 2 ) ) ) THEN
        !  Integrate base geopotential, starting at terrain elevation.  This assures that
        !  the base state is in exact hydrostatic balance with respect to the model equations.
        !  This field is on full levels.
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)               
        grid%phb(i,1,j) = grid%ht(i,j) * g
        IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
           DO kk  = 2,kte
              k = kk - 1
              grid%phb(i,kk,j) = grid%phb(i,kk-1,j) - grid%dnw(kk-1)*(grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))*grid%alb(i,kk-1,j)
           END DO
        ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
           DO k  = 2,kte
              pfu = grid%c3f(k  )*grid%MUB(i,j) + grid%c4f(k  ) + grid%p_top
              pfd = grid%c3f(k-1)*grid%MUB(i,j) + grid%c4f(k-1) + grid%p_top
              phm = grid%c3h(k-1)*grid%MUB(i,j) + grid%c4h(k-1) + grid%p_top
              grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
            END DO
          ENDIF
       ENDDO
       ENDDO
       END IF
     ELSE
        IF ( config_flags%ideal_init_method .EQ. 1 ) THEN
           DO j = jts,min(jte,jde-1)
           DO k = kts,kte-1
           DO i = its, min(ite,ide-1)
             IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
               grid%pb(i,k,j) = grid%c3h(k  )*grid%MUB(i,j) + grid%c4h(k  ) + grid%p_top
               grid%alb(i,k,j) = -grid%rdnw(k)*(grid%phb(i,k+1,j)-grid%phb(i,k,j))/(grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))
               grid%t_init(i,k,j) = grid%alb(i,k,j)*(p1000mb/r_d)/((grid%pb(i,k,j)/p1000mb)**cvpm) - t0
             ENDIF
           ENDDO
           ENDDO
           ENDDO
        ELSE IF ( config_flags%ideal_init_method .EQ. 2 ) THEN
           DO j = jts,min(jte,jde-1)
           DO k = kts,kte-1
           DO i = its, min(ite,ide-1)
             IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
               grid%pb(i,k,j) = grid%c3h(k  )*grid%MUB(i,j) + grid%c4h(k  ) + grid%p_top
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
             ENDIF
           ENDDO
           ENDDO
           ENDDO
           !  Integrate base geopotential, starting at terrain elevation.  This assures that
           !  the base state is in exact hydrostatic balance with respect to the model equations.
           !  This field is on full levels.
           DO j = jts,min(jte,jde-1)
           DO i = its, min(ite,ide-1)
            grid%phb(i,1,j) = grid%ht(i,j) * g
            DO kk  = 2,kte
              k = kk - 1
              grid%phb(i,kk,j) = grid%phb(i,kk-1,j) - grid%dnw(kk-1)*(grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))*grid%alb(i,kk-1,j)
            END DO
           ENDDO
           ENDDO
        END IF  ! end if initialization for ideal, use method 1 or 2
     END IF ! end if this is a real or ideal run
    END IF ! end if there is more than 1 domain
     
!------base state is finished, perturbation values are recalculated below-----
  ! For the HRRR application, we want to go through this rebalancing.  They
  ! have a clunky way of testig on this, but it is operational, so no need to
  ! cause them troubles.
  IF ( ( grid%dfi_opt .EQ. DFI_NODFI ) .and. ( config_flags%cycling ) ) THEN
       call rebalance_driver_cycl (grid )
       DO j = jts,min(jte,jde-1)
       DO k = kts,kte
       DO i = its, min(ite,ide-1)
          grid%ph_1(i,k,j)=grid%ph_2(i,k,j)
       ENDDO
       ENDDO
       ENDDO
  !  Everyone else runs thorugh here.  NOTE that we might also want to call the
  !  rebalancing ourselves for different set ups.  It does not impact HRRR, so
  !  it is an easy decision.
  ELSE
     ! We request rebalancing for vertical grid nesting, or when the user asks for rebalancing.
     ! Rebalance recomputes 1/rho, p, ph_2, ph0, p_hyd
     IF ( ( config_flags%rebalance .EQ. 1 ) .OR. &
        ( ( config_flags%rebalance .EQ. 2 ) .AND. ( config_flags%vert_refine_method .EQ. 2 ) ) ) THEN
       call rebalance_driver_cycl (grid )
       DO j = jts,min(jte,jde-1)
       DO k = kts,kte
       DO i = its, min(ite,ide-1)
          grid%ph_1(i,k,j)=grid%ph_2(i,k,j)
       ENDDO
       ENDDO
       ENDDO
     END IF
     ! Use equations from calc_p_rho_phi to derive p and al from ph: linear in log p
    IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
       DO j=jts,min(jte,jde-1)
       DO k=kts,kte-1
       DO i=its,min(ite,ide-1)
          grid%al(i,k,j)=-1./((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_1(i,j)))*(grid%alb(i,k,j)*(grid%c1h(k)*grid%mu_1(i,j))  &
                     +grid%rdnw(k)*(grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j)))
       ENDDO
       ENDDO
       ENDDO
    ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
       DO j=jts,min(jte,jde-1)
       DO k=kts,kte-1
       DO i=its,min(ite,ide-1)
          pfu = grid%c3f(k+1)*(grid%MUB(i,j)+grid%MU_1(i,j)) + grid%c4f(k+1) + grid%p_top
          pfd = grid%c3f(k  )*(grid%MUB(i,j)+grid%MU_1(i,j)) + grid%c4f(k  ) + grid%p_top
          phm = grid%c3h(k  )*(grid%MUB(i,j)+grid%MU_1(i,j)) + grid%c4h(k  ) + grid%p_top
          grid%al(i,k,j) = (grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j)+grid%phb(i,k+1,j)-grid%phb(i,k,j)) &
                            /phm/LOG(pfd/pfu)-grid%alb(i,k,j)
       ENDDO
       ENDDO
       ENDDO
    END IF ! which hypsometric option
    DO j=jts,min(jte,jde-1)
    DO k=kts,kte-1
    DO i=its,min(ite,ide-1)
#if ( WRFPLUS == 1 )
       IF ( .NOT. config_flags%var4d_run ) THEN
          IF ( config_flags%use_theta_m == 0 ) THEN
             qvf = 1.+rvovrd*moist(i,k,j,P_QV)
          ELSE
             qvf = 1
          END IF
          grid%p(i,k,j)=p1000mb*( (r_d*(t0+grid%t_1(i,k,j))*qvf)/                     &
                     (p1000mb*(grid%al(i,k,j)+grid%alb(i,k,j))) )**cpovcv  &
                     -grid%pb(i,k,j)
       END IF
#else
       IF ( config_flags%use_theta_m == 0 ) THEN
          qvf = 1.+rvovrd*moist(i,k,j,P_QV)
       ELSE
          qvf = 1
       END IF
       grid%p(i,k,j)=p1000mb*( (r_d*(t0+grid%t_1(i,k,j))*qvf)/                     &
                  (p1000mb*(grid%al(i,k,j)+grid%alb(i,k,j))) )**cpovcv  &
                  -grid%pb(i,k,j)
#endif
       grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
       grid%alt(i,k,j) = grid%al(i,k,j) + grid%alb(i,k,j)
    ENDDO
    ENDDO
    ENDDO
  ENDIF ! Various rebalancing options
    IF ( .NOT. grid%this_is_an_ideal_run ) THEN
       DO j=jts,min(jte,jde-1)
          DO i=its,min(ite,ide-1)
             p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
             grid%p_hyd_w(i,1,j) = grid%p(i,1,j) + grid%c3f(1)*(p_surf - grid%p_top) + grid%c4f(1) + grid%p_top
             DO k=kts+1,kte
                grid%p_hyd_w(i,k,j) = ( 2.*(grid%p(i,k-1,j)+grid%pb(i,k-1,j)) - grid%p_hyd_w(i,k-1,j) )
             ENDDO
          ENDDO
       ENDDO
    ELSE
       DO j=jts,min(jte,jde-1)
          DO i=its,min(ite,ide-1)
             p_surf = grid%MUB(i,j)+grid%p_top
             grid%p_hyd_w(i,1,j) = grid%p(i,1,j) + grid%c3f(1)*(p_surf - grid%p_top) + grid%c4f(1) + grid%p_top
             DO k=kts+1,kte
                grid%p_hyd_w(i,k,j) = ( 2.*(grid%p(i,k-1,j)+grid%pb(i,k-1,j)) - grid%p_hyd_w(i,k-1,j) )
             ENDDO
          ENDDO
       ENDDO
    END IF

   ENDIF ! first trip for this domain

   DO j=jts,min(jte,jde-1)
      DO k = kts,kte
         DO i = its, min(ite,ide-1)
            z_at_q(i,k,j)=(grid%ph_2(i,k,j)+grid%phb(i,k,j))/g
         ENDDO
      ENDDO
   ENDDO

   IF ( grid%press_adj .and. ( grid%id .NE. 1 ) .AND. .NOT. ( config_flags%restart ) .AND. &
       ( ( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ) ) ) THEN
      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            grid%MU_2(i,j) = grid%MU_2(i,j) + grid%al(i,1,j) / ( grid%alt(i,1,j) * grid%alb(i,1,j) ) * &
                                    g * ( grid%ht(i,j) - grid%ht_fine(i,j) )
         END DO
       END DO
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
          grid%MU_1(i,j)=grid%MU_2(i,j)
       ENDDO
       ENDDO

   END IF

! set GMT outside of phy_init because phy_init may not be called on this
! process if, for example, it is a moving nest and if this part of the domain is not
! being initialized (not the leading edge).
   CALL domain_setgmtetc( grid, start_of_simulation )
   IF ( first_trip_for_this_domain ) THEN

   CALL wrf_debug ( 100 , 'start_domain_em: Before call to phy_init' )

! namelist MPDT does not exist yet, so set it here
! MPDT is the call frequency for microphysics in minutes (0 means every step)
   MPDT = 0.

   IF(config_flags%cycling) THEN
       start_of_simulation = .true.
!  print *,'cycling, start_of_simulation -->',config_flags%cycling, start_of_simulation
       grid%xtime=0.
!   print *,'xtime=',grid%xtime
   ENDIF
!-----------------------------------------------------------------------------
! Adaptive time step: Added by T. Hutchinson, WSI  11/6/07
!
!

   IF ( ( grid%use_adaptive_time_step ) .AND. &
        ( ( grid%dfi_opt .EQ. DFI_NODFI ) .OR. ( grid%dfi_stage .EQ. DFI_FST ) ) ) THEN

!  Calculate any variables that were not set
!BPR BEGIN
!     This subroutine is called more than once at the first time step for a
!     given domain.  The following if statement is to prevent the code in
!     the if statement from executing more than once per domain at the
!     beginning of the model run (since last_step_update=-1 the first time
!     this is reached and should be =0 after this).
!     Without this if statement, when this code was executed for the second
!     time it can result in grid%dt being set incorrectly.
!     -This is because grid%dt will be set equal to grid%starting_time_step
!      which ignores a possible denominator in the starting time step.
!     -The first time this code is reached is also does this, but then the
!      call to adapt_timestep correct this
!     -Subsequent times this code is reached adapt_timestep will not correct
!      this because it will recognize that it has already been executed for
!      this timestep and exit out before doing the calculation.

      if (grid%last_step_updated .NE. grid%itimestep) then

      if (grid%starting_time_step == -1) then
         grid%starting_time_step = NINT(4 * MIN(grid%dx,grid%dy) / 1000)
      endif

      grid%time_step = grid%starting_time_step
      config_flags%time_step = grid%starting_time_step
      model_config_rec%time_step = grid%starting_time_step

      if (grid%max_time_step == -1) then
         grid%max_time_step = NINT(8 * MIN(grid%dx,grid%dy) / 1000)
      endif

      if (grid%min_time_step == -1) then
         grid%min_time_step = NINT(3 * MIN(grid%dx,grid%dy) / 1000)
      endif

!     Set a starting timestep.

      grid%dt = grid%starting_time_step

!     Initialize max cfl values
      
      grid%last_max_vert_cfl = 0
      grid%last_max_horiz_cfl = 0

!     Check to assure that time_step_sound is to be dynamically set.

      CALL nl_set_time_step_sound ( 1 , 0 )
      grid%time_step_sound = 0

      grid%max_msftx=MAXVAL(grid%msftx)
      grid%max_msfty=MAXVAL(grid%msfty)
#ifdef DM_PARALLEL
      CALL wrf_dm_maxval(grid%max_msftx, idex, jdex)
      CALL wrf_dm_maxval(grid%max_msfty, idex, jdex)
#endif
      end if
!BPR END

!     This first call just initializes variables.
!     If a restart, get initialized variables from restart file

      IF ( .NOT. ( config_flags%restart ) ) then
         CALL adapt_timestep(grid, config_flags)
      END IF


   END IF  ! adaptive computations

! End of adaptive time step modifications
!-----------------------------------------------------------------------------


   CALL set_tiles ( grid , grid%imask_nostag, ims, ime, jms, jme, ips, ipe, jps, jpe )
!
! Phy init can do reads and broadcasts when initializing physics -- landuse for example. However, if
! we're running on a reduced mesh (that is, some tasks don't have any work) we have to at least let them
! pass through this code so the broadcasts don't hang on the other, active tasks.  Set the number of
! tiles to a minimum of 1 and assume that the backwards patch ranges (ips=0, ipe=-1) will prevent
! anything else from happening on the blank tasks.  JM 20080605
!
   if ( allowed_to_read ) grid%num_tiles = max(1,grid%num_tiles)
!
! Phy_init is not necessarily thread-safe; do not multi-thread this loop.
! The tiling is to handle the fact that we may be masking off part of the computation.
!

#ifdef DM_PARALLEL
if(config_flags%sf_surface_physics.eq.NOAHMPSCHEME.and.config_flags%opt_run.eq.5)then
# include "HALO_EM_HYDRO_NOAHMP_INIT.inc"
endif
#endif

    IF ( ( grid%dfi_opt .EQ. DFI_NODFI ) .or. &
          ( ( grid%dfi_stage .NE. DFI_BCK ) .and. &
          ( grid%dfi_stage .NE. DFI_STARTBCK ) ) ) THEN

   DO ij = 1, grid%num_tiles

     CALL phy_init (  grid%id , config_flags, grid%DT, grid%RESTART, grid%znw, grid%znu,   &
                      grid%p_top, grid%tsk, grid%RADT,grid%BLDT,grid%CUDT, MPDT, &
                      grid%rucuten, grid%rvcuten, grid%rthcuten,             &
                      grid%rqvcuten, grid%rqrcuten, grid%rqccuten,           &
                      grid%rqscuten, grid%rqicuten,                          &
                      grid%rushten, grid%rvshten, grid%rthshten,             &
                      grid%rqvshten, grid%rqrshten, grid%rqcshten,           &
                      grid%rqsshten, grid%rqishten, grid%rqgshten,           &
                      grid%rublten,grid%rvblten,grid%rthblten,               &
                      grid%rqvblten,grid%rqcblten,grid%rqiblten,             &
                      grid%rthraten,grid%rthratenlw,grid%rthratensw,         &
                      !BSINGH - For WRFCuP scheme(11/12/2013)
                      grid%cupflag,grid%cldfra_cup,grid%cldfratend_cup,      & !wig, 18-Sep-2006
                      grid%shall,                                            & !wig, 18-Sep-2006
                      grid%tcloud_cup,                                       & !rce, 18-apr-2012
                      !BSINGH - ENDS
                      grid%stepbl,grid%stepra,grid%stepcu,                   &
                      grid%w0avg, grid%rainnc, grid%rainc, grid%raincv, grid%rainncv,  &
                      grid%snownc, grid%snowncv, grid%graupelnc, grid%graupelncv,  &
                      z_at_q, grid%alt, grid%qnwfa2d, scalar(ims,kms,jms,1), num_scalar,         & ! G. Thompson
                      grid%re_cloud, grid%re_ice, grid%re_snow,         & ! G. Thompson
                      grid%has_reqc, grid%has_reqi, grid%has_reqs,      & ! G. Thompson
                      grid%re_cloud_gsfc, grid%re_ice_gsfc,             & ! Goddard
                      grid%re_snow_gsfc, grid%re_graupel_gsfc,          & ! Goddard
                      grid%re_hail_gsfc, grid%re_rain_gsfc,             & ! Goddard
                      grid%nca,grid%swrad_scat,                    &
                      grid%cldefi,grid%lowlyr,                          &
                      grid%mass_flux,                              &
                      grid%rthften, grid%rqvften,                       &
                      grid%cldfra,                                      &
#if (WRF_CHEM == 1)
                      grid%cldfra_old,                                  &
#else
                      cldfra_old,                                  &
#endif
                      grid%glw,grid%gsw,grid%emiss,grid%embck,            &
                      grid%lu_index,                                      &
                      grid%landuse_ISICE, grid%landuse_LUCATS,            &
                      grid%landuse_LUSEAS, grid%landuse_ISN,              &
                      grid%lu_state,                                      &
                      grid%xlat,grid%xlong,grid%xlong_u,grid%xlat_v,grid%albedo,grid%albbck,grid%GMT,grid%JULYR,grid%JULDAY,     &
                      grid%levsiz, num_ozmixm, num_aerosolc, grid%paerlev,  &
                      grid%alevsiz, grid%no_src_types,                      &
                      grid%tmn,grid%xland,grid%znt,grid%z0,grid%ust,grid%mol,grid%pblh,grid%tke_pbl,    &
                      grid%exch_h,grid%thc,grid%snowc,grid%mavail,grid%hfx,grid%qfx,grid%rainbl, &
                      grid%tslb,grid%zs,grid%dzs,config_flags%num_soil_layers,grid%warm_rain,  &
                      grid%adv_moist_cond, grid%is_CAMMGMP_used,                               &
                      grid%apr_gr,grid%apr_w,grid%apr_mc,grid%apr_st,grid%apr_as,      &
                      grid%apr_capma,grid%apr_capme,grid%apr_capmi,          &
                      grid%xice,grid%xicem,grid%vegfra,grid%snow,grid%canwat,grid%smstav,         &
                      grid%smstot, grid%sfcrunoff,grid%udrunoff,grid%grdflx,grid%acsnow,      &
                      grid%acsnom,grid%ivgtyp,grid%isltyp, grid%sfcevp,grid%smois,     &
                      grid%sh2o, grid%snowh, grid%smfr3d,                    &
                      grid%snoalb,                 &
                      grid%DX,grid%DY,grid%f_ice_phy,grid%f_rain_phy,grid%f_rimef_phy, &
                      grid%mp_restart_state,grid%tbpvs_state,grid%tbpvs0_state,&
                      allowed_to_read, grid%moved, start_of_simulation,               &
                      grid%LAGDAY, &
                      ids, ide, jds, jde, kds, kde,           &
                      ims, ime, jms, jme, kms, kme,           &
                      grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), kts, kte, &
                      config_flags%num_urban_ndm,                           & !multi-layer urban
                      config_flags%urban_map_zrd,                           & !multi-layer urban
                      config_flags%urban_map_zwd,                           & !multi-layer urban
                      config_flags%urban_map_gd,                            & !multi-layer urban
                      config_flags%urban_map_zd,                            & !multi-layer urban
                      config_flags%urban_map_zdf,                           & !multi-layer urban
                      config_flags%urban_map_bd,                            & !multi-layer urban
                      config_flags%urban_map_wd,                            & !multi-layer urban
                      config_flags%urban_map_gbd,                           & !multi-layer urban
                      config_flags%urban_map_fbd,                           & !multi-layer urban
                      config_flags%num_urban_hi,                            & !multi-layer urban
                      grid%raincv_a,grid%raincv_b,                                    &
                      grid%gd_cloud, grid%gd_cloud2,                                  & ! Optional
                      grid%gd_cloud_a, grid%gd_cloud2_a,                              & ! Optional
                      grid%QC_CU, grid%QI_CU,                                         & ! Optional
                      ozmixm,grid%pin,                             &     ! Optional
                      grid%aerodm,grid%pina,                       &     ! Optional
                      grid%m_ps_1,grid%m_ps_2,grid%m_hybi,aerosolc_1,aerosolc_2,&  ! Optional
                      grid%rundgdten,grid%rvndgdten,grid%rthndgdten,         &     ! Optional
                      grid%rphndgdten,grid%rqvndgdten,grid%rmundgdten,       &     ! Optional
                      grid%SDA_HFX, grid%SDA_QFX, grid%QNORM, grid%HFX_BOTH,grid%QFX_BOTH,      &   ! fasdas
                      grid%HFX_FDDA,                                                            &   ! fasdas
                      grid%FGDT,grid%stepfg,                        &     ! Optional
                      grid%cugd_tten,grid%cugd_ttens,grid%cugd_qvten,        &   ! Optional
                      grid%cugd_qvtens,grid%cugd_qcten,                 &   ! Optional
                      grid%ISNOWXY, grid%ZSNSOXY, grid%TSNOXY,                                  &   ! Optional Noah-MP
                      grid%SNICEXY, grid%SNLIQXY, grid%TVXY, grid%TGXY, grid%CANICEXY,          &   ! Optional Noah-MP
                      grid%CANLIQXY, grid%EAHXY, grid%TAHXY, grid%CMXY,                         &   ! Optional Noah-MP
                      grid%CHXY, grid%FWETXY, grid%SNEQVOXY, grid%ALBOLDXY, grid%QSNOWXY,       &   ! Optional Noah-MP
                      grid%WSLAKEXY, grid%ZWTXY, grid%WAXY, grid%WTXY, grid%LFMASSXY, grid%RTMASSXY, & ! Optional Noah-MP
                      grid%STMASSXY, grid%WOODXY, grid%STBLCPXY, grid%FASTCPXY,                 &   ! Optional Noah-MP
                      grid%GRAINXY, grid%GDDXY,                                                 &   ! Optional Noah-MP
                      grid%CROPTYPE, grid%CROPCAT,                                                 &   ! Optional Noah-MP
                      config_flags%opt_crop,                                                    &
                      grid%gecros_state,                                                        &   ! Optional gecros crop
                      grid%XSAIXY,grid%LAI,                                                     &   ! Optional Noah-MP
                      grid%T2MVXY, grid%T2MBXY, grid%CHSTARXY,                                  &   ! Optional Noah-MP
                      grid%SMOISEQ  ,grid%SMCWTDXY ,grid%RECHXY, grid%DEEPRECHXY, grid%AREAXY,  & ! Optional Noah-MP
                      config_flags%wtddt ,grid%stepwtd ,grid%QRFSXY ,grid%QSPRINGSXY ,grid%QSLATXY, & ! Optional Noah-MP
                      grid%FDEPTHXY, grid%RIVERBEDXY, grid%EQZWT, grid%RIVERCONDXY, grid%PEXPXY, & ! Optional Noah-MP
                      grid%rechclim  ,                                                           & ! Optional Noah-MP
                      grid%msftx, grid%msfty,                              &
                      grid%DZR, grid%DZB, grid%DZG,                          & !Optional urban
                      grid%TR_URB2D,grid%TB_URB2D,grid%TG_URB2D,grid%TC_URB2D,    & !Optional urban
                      grid%QC_URB2D, grid%XXXR_URB2D,grid%XXXB_URB2D,        & !Optional urban
                      grid%XXXG_URB2D, grid%XXXC_URB2D,                 & !Optional urban
                      grid%TRL_URB3D, grid%TBL_URB3D, grid%TGL_URB3D,        & !Optional urban
                      grid%SH_URB2D, grid%LH_URB2D, grid%G_URB2D, grid%RN_URB2D,  & !Optional urban
                      grid%TS_URB2D, grid%FRC_URB2D, grid%UTYPE_URB2D,      & !Optional urban
                      grid%CMCR_URB2D,grid%TGR_URB2D,grid%TGRL_URB3D,grid%SMR_URB3D, & !Optional urban   
                      grid%DRELR_URB2D,grid%DRELB_URB2D,grid%DRELG_URB2D,    & !Optional urban
                      grid%FLXHUMR_URB2D,grid%FLXHUMB_URB2D,grid%FLXHUMG_URB2D,   & !Optional urban
                      grid%TRB_URB4D,grid%TW1_URB4D,grid%TW2_URB4D,grid%TGB_URB4D,grid%TLEV_URB3D,  & !multi-layer urban
                      grid%QLEV_URB3D,grid%TW1LEV_URB3D,grid%TW2LEV_URB3D,        & !multi-layer urban
                      grid%TGLEV_URB3D,grid%TFLEV_URB3D,grid%SF_AC_URB3D,         & !multi-layer urban
                      grid%LF_AC_URB3D,grid%CM_AC_URB3D,grid%SFVENT_URB3D,grid%LFVENT_URB3D,   & !multi-layer urban
                      grid%SFWIN1_URB3D,grid%SFWIN2_URB3D, & !multi-layer urban
                      grid%SFW1_URB3D,grid%SFW2_URB3D,grid%SFR_URB3D,grid%SFG_URB3D,     & !multi-layer urban
                      grid%LP_URB2D,grid%HI_URB2D,grid%LB_URB2D,grid%HGT_URB2D,        & !multi-layer urban
                      grid%MH_URB2D,grid%STDH_URB2D,grid%LF_URB2D,                     & !SLUCM
                      grid%A_U_BEP,grid%A_V_BEP,grid%A_T_BEP,grid%A_Q_BEP,             & !multi-layer urban
                      grid%A_E_BEP,grid%B_U_BEP,grid%B_V_BEP,grid%B_T_BEP,             & !multi-layer urban
                      grid%B_Q_BEP,grid%B_E_BEP,grid%DLG_BEP,                          & !multi-layer urban
                      grid%DL_U_BEP,grid%SF_BEP,grid%VL_BEP,                           & !multi-layer urban
                      grid%TML,grid%T0ML,grid%HML,grid%H0ML,grid%HUML,grid%HVML,grid%TMOML,     & !Optional oml
                      grid%lakedepth2d,  grid%savedtke12d,  grid%snowdp2d,   grid%h2osno2d,       & !lake
                      grid%snl2d,        grid%t_grnd2d,     grid%t_lake3d,   grid%lake_icefrac3d, & !lake
                      grid%z_lake3d,     grid%dz_lake3d,    grid%t_soisno3d, grid%h2osoi_ice3d,   & !lake
                      grid%h2osoi_liq3d, grid%h2osoi_vol3d, grid%z3d,        grid%dz3d,           & !lake
                      grid%zi3d,         grid%watsat3d,     grid%csol3d,     grid%tkmg3d,         & !lake
                      grid%tkdry3d,      grid%tksatu3d,     grid%lake2d,                            & !lake
                      config_flags%lakedepth_default,        config_flags%lake_min_elev, grid%lake_depth,     & !lake
                      grid%lakemask,        grid%lakeflag,  grid%LAKE_DEPTH_FLAG, grid%use_lakedepth,     & !lake
                      config_flags%sf_surface_mosaic, config_flags%mosaic_cat, config_flags%num_land_cat, & ! Noah tiling
                      config_flags%maxpatch,           &   ! start of CLM variables
              grid%numc,grid%nump,grid%snl,grid%snowdp,&   !
              grid%wtc,grid%wtp,&
              grid%h2osno,grid%t_grnd,grid%t_veg,grid%h2ocan, &
              grid%h2ocan_col,grid%t2m_max,grid%t2m_min,&
              grid%t_ref2m,&
              grid%h2osoi_liq_s1,&
              grid%h2osoi_liq_s2,grid%h2osoi_liq_s3,&
              grid%h2osoi_liq_s4, &
              grid%h2osoi_liq_s5, &
              grid%h2osoi_liq1,grid%h2osoi_liq2,&
              grid%h2osoi_liq3,grid%h2osoi_liq4,&
              grid%h2osoi_liq5,grid%h2osoi_liq6, &
              grid%h2osoi_liq7,grid%h2osoi_liq8,&
              grid%h2osoi_liq9,   &
              grid%h2osoi_liq10, &
              grid%h2osoi_ice_s1,grid%h2osoi_ice_s2,&
              grid%h2osoi_ice_s3, &
              grid%h2osoi_ice_s4, &
              grid%h2osoi_ice_s5, &
              grid%h2osoi_ice1,&
              grid%h2osoi_ice2,&
              grid%h2osoi_ice3,grid%h2osoi_ice4,&
              grid%h2osoi_ice5, &
              grid%h2osoi_ice6, &
              grid%h2osoi_ice7,grid%h2osoi_ice8,&
              grid%h2osoi_ice9,grid%h2osoi_ice10,&
              grid%t_soisno_s1,grid%t_soisno_s2, &
              grid%t_soisno_s3,grid%t_soisno_s4, &
              grid%t_soisno_s5,grid%t_soisno1,&
              grid%t_soisno2,grid%t_soisno3,&
              grid%t_soisno4,grid%t_soisno5, &
              grid%t_soisno6,grid%t_soisno7,&
              grid%t_soisno8,grid%t_soisno9,&
              grid%t_soisno10,grid%dzsnow1,grid%dzsnow2,grid%dzsnow3,grid%dzsnow4,&
              grid%dzsnow5,grid%snowrds1,grid%snowrds2,grid%snowrds3,grid%snowrds4,grid%snowrds5, &
              grid%t_lake1,grid%t_lake2,&
              grid%t_lake3,grid%t_lake4, &
              grid%t_lake5,grid%t_lake6, &
              grid%t_lake7,grid%t_lake8, &
              grid%t_lake9,grid%t_lake10,&
              grid%h2osoi_vol1,grid%h2osoi_vol2, &
              grid%h2osoi_vol3,grid%h2osoi_vol4, &
              grid%h2osoi_vol5,&
              grid%h2osoi_vol6,&
              grid%h2osoi_vol7,grid%h2osoi_vol8,&
              grid%h2osoi_vol9,grid%h2osoi_vol10,&
              grid%ht,                          &
              grid%ALBEDOsubgrid,grid%LHsubgrid,&
              grid%HFXsubgrid,grid%LWUPsubgrid,&
              grid%Q2subgrid,grid%SABVsubgrid,  &
              grid%SABGsubgrid,grid%NRAsubgrid,&
              grid%SWUPsubgrid,grid%lhsoi, &
              grid%lhveg, grid%lhtran, & !end of CLM variables
                      grid%TSK_SAVE,                        & !Optional fractional seaice
                      grid%itimestep, grid%fdob,            &
                      t00, p00, a,                      & ! for obs_nudge base state
                      grid%TYR, grid%TYRA, grid%TDLY, grid%TLAG, grid%NYEAR, grid%NDAY,grid%tmn_update, &
                      grid%achfx, grid%aclhf, grid%acgrdflx,                  &
                      config_flags%nssl_cccn,                                 &
                      config_flags%nssl_alphah, config_flags%nssl_alphahl,    &
                      config_flags%nssl_cnoh, config_flags%nssl_cnohl,        &
                      config_flags%nssl_cnor, config_flags%nssl_cnos,         &
                      config_flags%nssl_rho_qh, config_flags%nssl_rho_qhl,    &
                      config_flags%nssl_rho_qs,                               &
                      config_flags%nssl_ipelec,                               &
                      config_flags%nssl_isaund                               &
                      ,grid%RQCNCUTEN, grid%RQINCUTEN,grid%rliq             &      !mchen add for cammpmg
                      ,grid%cldfra_dp,grid%cldfra_sh                        & ! ckay for subgrid cloud
                      ,grid%te_temf,grid%cf3d_temf,grid%wm_temf        & ! WA
                      ,grid%massflux_EDKF, grid%entr_EDKF, grid%detr_EDKF                &
                      ,grid%thl_up,grid%thv_up,grid%rt_up                                &
                      ,grid%rv_up,grid%rc_up,grid%u_up,grid%v_up,grid%frac_up            &
                      ,grid%RDCASHTEN, grid%RQCDCSHTEN                                   &
                      ,grid%cldareaa, grid%cldareab, grid%cldliqa, grid%cldliqb          &
                      ,grid%ca_rad, grid%cw_rad                                          &
                      ,grid%pblmax, grid%wub, grid%ltopb, grid%clddpthb, grid%cldtopb    &
                      ,grid%capesave, grid%ainckfsa, grid%radsave                        &
                      ,grid%rainsh, grid%rainshvb, grid%kdcldtop, grid%kdcldbas          &
                      ,grid%xtime1, grid%PBLHAVG, grid%TKEAVG                            &
                      ,grid%ccn_conc                                          & ! RAS
                      ,grid%QKE                                               &!JOE-for mynn
                      ,grid%landusef,grid%landusef2,grid%mosaic_cat_index                            & ! danli mosaic
                      ,grid%TSK_mosaic,grid%TSLB_mosaic,grid%SMOIS_mosaic,grid%SH2O_mosaic           & ! danli mosaic
                      ,grid%CANWAT_mosaic,grid%SNOW_mosaic,grid%SNOWH_mosaic,grid%SNOWC_mosaic       & ! danli mosaic
                      ,grid%ALBEDO_mosaic,grid%ALBBCK_mosaic, grid%EMISS_mosaic                      & ! danli mosaic
                      ,grid%EMBCK_mosaic, grid%ZNT_mosaic, grid%Z0_mosaic                            & ! danli mosaic
                      ,grid%TR_URB2D_mosaic,grid%TB_URB2D_mosaic                                     & ! danli mosaic
                      ,grid%TG_URB2D_mosaic,grid%TC_URB2D_mosaic                                     & ! danli mosaic
                      ,grid%QC_URB2D_mosaic                                                          & ! danli mosaic
                      ,grid%TRL_URB3D_mosaic,grid%TBL_URB3D_mosaic                                   & ! danli mosaic
                      ,grid%TGL_URB3D_mosaic                                                         & ! danli mosaic
                      ,grid%SH_URB2D_mosaic,grid%LH_URB2D_mosaic                                     & ! danli mosaic
                      ,grid%G_URB2D_mosaic,grid%RN_URB2D_mosaic                                      & ! danli mosaic
                      ,grid%TS_URB2D_mosaic                                                          & ! danli mosaic
                      ,grid%TS_RUL2D_mosaic                                                          & ! danli mosaic
                      ,grid%QR_CU, grid%QS_CU                                                        &
                      ,grid%NC_CU, grid%NI_CU, grid%NR_CU, grid%NS_CU,grid%CCN_CU                    &
                      ,grid%alevsiz_cu,grid%num_months,grid%no_src_types_cu,grid%aeromcu             &
                      ,grid%aeropcu,grid%EFCG,grid%EFCS,grid%EFIG,grid%EFIS,grid%EFSG,grid%EFSS      &
                      )
       ENDDO ! loop of tiles for phy_init

       if(.not.grid%restart) then

         if(config_flags%sf_surface_physics .eq. NOAHMPSCHEME .and. config_flags%opt_run .eq. 5) then
              call groundwater_init ( grid ,                                                            &
                 config_flags%num_soil_layers, grid%dzs, grid%isltyp, grid%ivgtyp, config_flags%wtddt , &
                 grid%fdepthxy   , grid%ht     , grid%riverbedxy, grid%eqzwt     ,                      &
                 grid%rivercondxy, grid%pexpxy , grid%areaxy    , grid%zwtxy     ,                      &
                 grid%smois      , grid%sh2o   , grid%smoiseq   , grid%smcwtdxy  , grid%deeprechxy,     &
                 grid%rechxy     , grid%qslatxy, grid%qrfsxy    , grid%qspringsxy,                      &
                 grid%rechclim   ,                                                                      &
                 ids,ide, jds,jde, kds,kde,                                                             &
                 ims,ime, jms,jme, kms,kme,                                                             &
                 ips,ipe, jps,jpe, kps,kpe,                                                             &
                 its,ite, jts,jte, kts,kte )
         endif

       endif

    ENDIF   ! no phy_init for the backwards part of the DFI

   CALL wrf_debug ( 100 , 'start_domain_em: After call to phy_init' )

   IF (config_flags%do_avgflx_em .EQ. 1) THEN
      WRITE ( message , FMT = '("start_em: initializing avgflx on domain ",I3)' ) &
           & grid%id
      CALL wrf_message(trim(message))
      grid%avgflx_count = 0
      f_flux = config_flags%do_avgflx_cugd .EQ. 1  ! WA 9/24/10
      DO ij = 1, grid%num_tiles
         call wrf_debug(200,'In start_em, before zero_avgflx call')
         if (.not. grid%restart) call zero_avgflx(grid%avgflx_rum,grid%avgflx_rvm,grid%avgflx_wwm, &
              &   ids, ide, jds, jde, kds, kde,           &
              &   ims, ime, jms, jme, kms, kme,           &
              &   grid%i_start(ij), grid%i_end(ij), grid%j_start(ij), grid%j_end(ij), kts, kte, f_flux, &
              &   grid%avgflx_cfu1,grid%avgflx_cfd1,grid%avgflx_dfu1, &
              &   grid%avgflx_efu1,grid%avgflx_dfd1,grid%avgflx_efd1 )
         call wrf_debug(200,'In start_em, after zero_avgflx call')
      ENDDO
   ENDIF

#ifdef MCELIO
   grid%LU_MASK = 0.
   WHERE ( grid%lu_index .EQ. 16 ) grid%LU_MASK = 1.
#endif

   call wrf_debug(100,'start_em: calling lightning_init')
   CALL lightning_init ( id=grid%id, itimestep=grid%itimestep, restart=grid%restart, dt=grid%dt, dx=grid%dx   &
                    ! Namelist control options
                        ,cu_physics=config_flags%cu_physics,mp_physics=config_flags%mp_physics    &
                        ,do_radar_ref=config_flags%do_radar_ref                                   &
                        ,lightning_option=config_flags%lightning_option                           &
                        ,lightning_dt=config_flags%lightning_dt                                   &
                        ,lightning_start_seconds=config_flags%lightning_start_seconds             &
                        ,ltngacttime=grid%ltngacttime                                             &
                        ,iccg_prescribed_num=config_flags%iccg_prescribed_num                     &
                        ,iccg_prescribed_den=config_flags%iccg_prescribed_den                     &
                        ,cellcount_method=config_flags%cellcount_method                           &
                    ! Order dependent args for domain, mem, and tile dims
                        ,ids=ids, ide=ide, jds=jds, jde=jde, kds=kds, kde=kde             &
                        ,ims=ims, ime=ime, jms=jms, jme=jme, kms=kms, kme=kme             &
                        ,its=its, ite=ite, jts=jts, jte=jte, kts=kts, kte=kte             &
                    ! IC and CG flash rates and accumulated flash count
                        ,ic_flashcount=grid%ic_flashcount, ic_flashrate=grid%ic_flashrate        &
                        ,cg_flashcount=grid%cg_flashcount, cg_flashrate=grid%cg_flashrate        &
#if (WRF_CHEM == 1)
                        ,lnox_opt=config_flags%lnox_opt           &
                        ,lnox_passive=config_flags%lnox_passive   &
                        ,lnox_total=tracer(:,:,:,p_lnox_total)    &
                        ,lnox_ic=tracer(:,:,:,p_lnox_ic)          &
                        ,lnox_cg=tracer(:,:,:,p_lnox_cg)          &
#endif
                    )

    call wrf_debug(100,'start_em: after calling lightning_init')

   END IF ! restart

#if 0
#include "CYCLE_TEST.inc"
#endif

!
!

 !  set physical boundary conditions for all initialized variables

!-----------------------------------------------------------------------
!  Stencils for patch communications  (WCS, 29 June 2001)
!  Note:  the size of this halo exchange reflects the
!         fact that we are carrying the uncoupled variables
!         as state variables in the mass coordinate model, as
!         opposed to the coupled variables as in the height
!         coordinate model.
!
!                           * * * * *
!         *        * * *    * * * * *
!       * + *      * + *    * * + * *
!         *        * * *    * * * * *
!                           * * * * *
!
!j  grid%u_1                          x
!j  grid%u_2                          x
!j  grid%v_1                          x
!j  grid%v_2                          x
!j  grid%w_1                          x
!j  grid%w_2                          x
!j  grid%t_1                          x
!j  grid%t_2                          x
!j grid%ph_1                          x
!j grid%ph_2                          x
!
!j  grid%t_init                       x
!
!j  grid%phb   x
!j  grid%ph0   x
!j  grid%php   x
!j   grid%pb   x
!j   grid%al   x
!j  grid%alt   x
!j  grid%alb   x
!
!  the following are 2D (xy) variables
!
!j grid%mu_1                          x
!j grid%mu_2                          x
!j grid%mub   x
!j grid%mu0   x
!j grid%ht    x
!j grid%msftx x
!j grid%msfty x
!j grid%msfux x
!j grid%msfuy x
!j grid%msfvx x
!j grid%msfvy x
!j grid%sina  x
!j grid%cosa  x
!j grid%e     x
!j grid%f     x
!
!  4D variables
!
! moist                        x
!  chem                        x
!scalar                        x

!--------------------------------------------------------------

#ifdef DM_PARALLEL
# include "HALO_EM_INIT_1.inc"
# include "HALO_EM_INIT_2.inc"
# include "HALO_EM_INIT_3.inc"
# include "HALO_EM_INIT_4.inc"
# include "HALO_EM_INIT_5.inc"
      IF ( config_flags%sf_ocean_physics .EQ. PWP3DSCHEME ) THEN
# include "HALO_EM_INIT_6.inc"
      END IF
# include "PERIOD_BDY_EM_INIT.inc"
# include "PERIOD_BDY_EM_MOIST.inc"
# include "PERIOD_BDY_EM_TKE.inc"
# include "PERIOD_BDY_EM_SCALAR.inc"
# include "PERIOD_BDY_EM_CHEM.inc"
if(config_flags%sf_surface_physics.eq.NOAHMPSCHEME.and.config_flags%opt_run.eq.5)then
# include "HALO_EM_HYDRO_NOAHMP_INIT.inc"
endif
#endif


   CALL set_physical_bc3d( grid%u_1 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%u_2 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%v_1 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%v_2 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

! set kinematic condition for w

   CALL set_physical_bc2d( grid%ht , 'r' , config_flags ,                &
                           ids , ide , jds , jde , &
                           ims , ime , jms , jme , &
                           its , ite , jts , jte , &
                           its , ite , jts , jte   )

! Set the w at the surface.  If this is the start of a forecast, or if this is a cycled run
! and the start of that forecast, we define the w field.  However, if this a restart, then
! we leave w alone.  Initial value is V dot grad(topo) at surface, then smoothly decaying
! above that.

   IF ( ( start_of_simulation .OR. config_flags%cycling ) .AND. ( .NOT. config_flags%restart ) ) THEN

! If W already exists (not zero), then we leave it alone.  How to do this?  We find the
! max/min on this node at the surface.  If parallel, we collect the max/min from all procs.
! If the max/min throughout the entire domain at the surface is identically 0, then we say
! that the W field is NOT initialized, and we run the set_w_surface routines for the
! two time levels of W.  If the field is already initialized, we do NOT run those two
! routines.

      w_max = grid%w_2(its,1,jts)
      w_min = grid%w_2(its,1,jts)
      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
         w_max = MAX ( w_max , grid%w_2(i,1,j) )
         w_min = MIN ( w_min , grid%w_2(i,1,j) )
         END DO
      END DO
#ifdef DM_PARALLEL
      w_max = wrf_dm_max_real ( w_max )
      w_min = wrf_dm_min_real ( w_min )
#endif

      IF ( ( ABS(w_max) .LT. 1.E-6 ) .AND. &
           ( ABS(w_min) .LT. 1.E-6 ) ) THEN
         w_needs_to_be_set = .TRUE.
         ! Force use_input_w to false, if there is no input w
#if ( WRFPLUS == 1 )
         IF ( config_flags%use_input_w ) THEN
            config_flags%use_input_w = .FALSE.
            CALL nl_set_use_input_w (grid%id, .FALSE.)
         ENDIF
#endif
      ELSE
         IF ( config_flags%use_input_w ) THEN
            w_needs_to_be_set = .FALSE.
         ELSE
            w_needs_to_be_set = .TRUE.
         END IF
      END IF

      IF ( w_needs_to_be_set ) THEN

         ! setting kinematic condition for w at the surface

         fill_w_flag = .true.
         CALL set_w_surface(  config_flags, grid%znw, fill_w_flag,             &
                              grid%w_1, grid%ht, grid%u_1, grid%v_1, grid%cf1, &
                              grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                              ids, ide, jds, jde, kds, kde,                    &
                              ims, ime, jms, jme, kms, kme,                    &
                              its, ite, jts, jte, kts, kte                     )
         CALL set_w_surface(  config_flags, grid%znw, fill_w_flag,             &
                              grid%w_2, grid%ht, grid%u_2, grid%v_2, grid%cf1, &
                              grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                              ids, ide, jds, jde, kds, kde,                    &
                              ims, ime, jms, jme, kms, kme,                    &
                              its, ite, jts, jte, kts, kte                     )
      END IF
   END IF

   IF ( .NOT. config_flags%restart ) THEN
! set up slope-radiation constant arrays based on topography
! paj: also computes laplacian for topo_wind
!
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
          im1 = max(i-1,ids)
          ip1 = min(i+1,ide-1)
          jm1 = max(j-1,jds)
          jp1 = min(j+1,jde-1)
          grid%toposlpx(i,j)=(grid%ht(ip1,j)-grid%ht(im1,j))*grid%msftx(i,j)*grid%rdx/(ip1-im1)
          grid%toposlpy(i,j)=(grid%ht(i,jp1)-grid%ht(i,jm1))*grid%msfty(i,j)*grid%rdy/(jp1-jm1)
          grid%lap_hgt(i,j)=(grid%ht(ip1,j)+grid%ht(im1,j)+grid%ht(i,jp1)+grid%ht(i,jm1)-grid%ht(i,j)*4.)/4.
             hx = grid%toposlpx(i,j)
             hy = grid%toposlpy(i,j)
             pi = 4.*atan(1.)
             grid%slope(i,j) = atan((hx**2+hy**2)**.5)
             if (grid%slope(i,j).lt.1.e-4) then
               grid%slope(i,j) = 0.
               grid%slp_azi(i,j) = 0.
             else
               grid%slp_azi(i,j) = atan2(hx,hy)+pi

! Rotate slope azimuth to lat-lon grid
               if (grid%cosa(i,j).ge.0) then
                 grid%slp_azi(i,j) = grid%slp_azi(i,j) - asin(grid%sina(i,j))
               else
                 grid%slp_azi(i,j) = grid%slp_azi(i,j) - (pi - asin(grid%sina(i,j)))
               endif
             endif
       ENDDO
       ENDDO
!
! paj: computes ct and ct2 for topo_wind
!
       grid%ctopo=1.
       grid%ctopo2=1.
!
       if (config_flags%topo_wind.eq.1) then
         DO j = jts,min(jte,jde-1)
         DO i = its, min(ite,ide-1)
            if(grid%xland(i,j).lt.1.5)grid%ctopo(i,j)=sqrt(grid%var_sso(i,j))
            if (grid%ctopo(i,j).le.2.718) then
            grid%ctopo(i,j)=1.
            else
            grid%ctopo(i,j)=alog(grid%ctopo(i,j))
            endif
!
            if (grid%lap_hgt(i,j).gt.-10.) then
              grid%ctopo(i,j)=grid%ctopo(i,j)
            else
               if (grid%lap_hgt(i,j).ge.-20) then
                 alpha=(grid%lap_hgt(i,j)+20.)/10.
                 grid%ctopo(i,j)=alpha*grid%ctopo(i,j)+(1-alpha)
               else
                  if (grid%lap_hgt(i,j).ge.-30.) then
                  grid%ctopo(i,j)=(grid%lap_hgt(i,j)+30.)/10.
                  grid%ctopo2(i,j)=(grid%lap_hgt(i,j)+30.)/10.
                  else
                  grid%ctopo(i,j)=0.
                  grid%ctopo2(i,j)=0.
                  endif
               endif
            endif
         ENDDO
         ENDDO
!       if topo_wind==2 (D.Ovens, C.Mass)
       else if (config_flags%topo_wind.eq.2) then
         DO j = jts,min(jte,jde-1)
         DO i = its, min(ite,ide-1)
           if (grid%xland(i,j).lt.1.5) then
              vfac = amin1(1.575,(grid%var2d(i,j)*0.4/200.+1.175))
              vfac = vfac * vfac
           else                      ! over water, leave it alone
              vfac = 1.
           endif
!          print *, j,i,grid%ctopo(i,j),vfac
           grid%ctopo(i,j)=grid%ctopo(i,j)*vfac
         ENDDO
         ENDDO
       endif

   END IF

   CALL set_physical_bc3d( grid%w_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%w_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%ph_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%ph_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%t_1 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%t_2 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc2d( grid%mu_1, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%mu_2, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%mub , 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
!  CALL set_physical_bc2d( grid%mu0 , 't' , config_flags ,   &
!                          ids , ide , jds , jde ,  &
!                          ims , ime , jms , jme ,  &
!                          its , ite , jts , jte ,  &
!                          its , ite , jts , jte   )


   CALL set_physical_bc3d( grid%phb , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%ph0 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%php , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL set_physical_bc3d( grid%pb , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%al , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%alt , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d( grid%alb , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d(grid%t_init, 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL set_physical_bc3d(grid%tke_2, 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   IF (num_moist > 0) THEN

! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_m   : DO loop = 1 , num_moist
         CALL set_physical_bc3d( moist(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_m

   ENDIF


! Some initializations for the simple ccn field.

   IF ( f_qnn ) THEN
      IF ( config_flags%mp_physics == wdm5scheme .or. config_flags%mp_physics == wdm6scheme ) THEN
         ! NO OP
      ELSE IF ( config_flags%mp_physics == nssl_2momccn ) THEN
         grid%ccn_conc =  config_flags%nssl_cccn/1.225
      ELSE
         ! NO OP
      END IF
      ccn_max_val = MAXVAL(scalar(its:MIN(ite,ide-1),kts:kte-1,jts:MIN(jte,jde-1),p_qnn))
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
      ccn_max_val = wrf_dm_max_real ( ccn_max_val )
#endif
      IF ( ccn_max_val < 1.0 ) THEN ! initialization of ccn not already done
         DO j=jts,MIN(jte,jde-1)
            DO k=kts,kte
               DO i=its,MIN(ite,ide-1)
                  scalar(i,k,j,p_qnn) = grid%ccn_conc
               END DO
            END DO
         END DO
      END IF
   END IF

   IF (num_scalar > 0) THEN

! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_s   : DO loop = 1 , num_scalar
         CALL set_physical_bc3d( scalar(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_s

   ENDIF


#if (WRF_CHEM == 1)
        if(config_flags%tracer_opt > 0 )then
           call initialize_tracer (tracer,config_flags%chem_in_opt, &
                               config_flags%tracer_opt,num_tracer,  &
                               ids,ide, jds,jde, kds,kde,      & ! domain dims
                               ims,ime, jms,jme, kms,kme,      & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,      & ! patch  dims
                               its,ite, jts,jte, kts,kte )

        endif
!
!   we do this here, so we only have one chem_init routine for either core....
!
         do j=jts,min(jte,jde-1)
            do i=its,min(ite,ide-1)
               do k=kts,kte
                 z_at_w(i,k,j)=(grid%ph_2(i,k,j)+grid%phb(i,k,j))/g
               enddo
               do k=kts,min(kte,kde-1)
                 tempfac=(grid%t_1(i,k,j) + t0)*((grid%p(i,k,j) + grid%pb(i,k,j))/p1000mb)**rcp
                 convfac(i,k,j) = (grid%p(i,k,j)+grid%pb(i,k,j))/rgasuniv/tempfac
               enddo
            enddo
         enddo

         CALL chem_init (grid%id,chem,emis_ant,scalar,grid%dt,grid%bioemdt,grid%photdt,     &
                         grid%chemdt,                                       &
                         grid%stepbioe,grid%stepphot,grid%stepchem,grid%stepfirepl,      &
                         grid%plumerisefire_frq,z_at_w,grid%xlat,grid%xlong,g,           &
                         grid%aerwrf,config_flags,grid,            &
                         grid%alt,grid%t_1,grid%p,convfac,grid%ttday,grid%tcosz,         &
                         grid%julday,grid%gmt,&
                         grid%tauaer1,grid%tauaer2,grid%tauaer3,grid%tauaer4,                   &
                         grid%gaer1,grid%gaer2,grid%gaer3,grid%gaer4,                           &
                         grid%waer1,grid%waer2,grid%waer3,grid%waer4,                           &
                         grid%l2aer,grid%l3aer,grid%l4aer,grid%l5aer,grid%l6aer,grid%l7aer,     &
                         grid%extaerlw1,grid%extaerlw2,grid%extaerlw3,grid%extaerlw4,           &
                         grid%extaerlw5,grid%extaerlw6,grid%extaerlw7,grid%extaerlw8,           &
                         grid%extaerlw9,grid%extaerlw10,grid%extaerlw11,grid%extaerlw12,        &
                         grid%extaerlw13,grid%extaerlw14,grid%extaerlw15,grid%extaerlw16,       &
                         grid%tauaerlw1,grid%tauaerlw2,grid%tauaerlw3,grid%tauaerlw4,           &
                         grid%tauaerlw5,grid%tauaerlw6,grid%tauaerlw7,grid%tauaerlw8,           &
                         grid%tauaerlw9,grid%tauaerlw10,grid%tauaerlw11,grid%tauaerlw12,        &
                         grid%tauaerlw13,grid%tauaerlw14,grid%tauaerlw15,grid%tauaerlw16,       &
                         grid%dgnum4d, grid%dgnumwet4d, grid%dgnum_a1, grid%dgnum_a2,           &
                         grid%dgnum_a3, grid%dgnumwet_a1, grid%dgnumwet_a2, grid%dgnumwet_a3,   &
                         grid%pm2_5_dry,grid%pm2_5_water,grid%pm2_5_dry_ec,                &
                         grid%tsoa,grid%asoa,grid%bsoa,                                    &
                         grid%last_chem_time_year,grid%last_chem_time_month,               &
                         grid%last_chem_time_day,grid%last_chem_time_hour,                 &
                         grid%last_chem_time_minute,grid%last_chem_time_second,            &
                         grid%chem_in_opt,grid%kemit,grid%num_vert_mix,                       &
                         ids , ide , jds , jde , kds , kde ,                &
                         ims , ime , jms , jme , kms , kme ,                &
                         its , ite , jts , jte , kts , kte                  )

!
! calculate initial pm
!
!       print *,'calculating initial pm'
        select case (config_flags%chem_opt)
        case (GOCART_SIMPLE,GOCARTRACM_KPP,GOCARTRADM2)
           call sum_pm_gocart (                                             &
                grid%alt, chem, grid%pm2_5_dry, grid%pm2_5_dry_ec,grid%pm10,&
                ids,ide, jds,jde, kds,kde,                                  &
                ims,ime, jms,jme, kms,kme,                                  &
                its,ite, jts,jte, kts,kte-1                                 )
        case (RADM2SORG,RADM2SORG_AQ,RADM2SORG_AQCHEM,RADM2SORG_KPP,RACMSORG_AQ,RACMSORG_AQCHEM_KPP, &
              RACM_ESRLSORG_AQCHEM_KPP,RACMSORG_KPP)
           call sum_pm_sorgam (                                             &
                grid%alt, chem, grid%h2oaj, grid%h2oai,                                    &
                grid%pm2_5_dry, grid%pm2_5_water, grid%pm2_5_dry_ec, grid%pm10,                 &
                grid%dust_opt,ids,ide, jds,jde, kds,kde,                    &
                ims,ime, jms,jme, kms,kme,                                  &
                its,ite, jts,jte, kts,kte-1                                 )
         case (RACM_SOA_VBS_KPP,RACM_SOA_VBS_AQCHEM_KPP)
             call sum_pm_soa_vbs (                                           &
                 grid%alt, chem, grid%h2oaj, grid%h2oai,                                   &
                 grid%pm2_5_dry, grid%pm2_5_water, grid%pm2_5_dry_ec, grid%pm10,                &
                 config_flags%dust_opt,ids,ide, jds,jde, kds,kde,           &
                 ims,ime, jms,jme, kms,kme,                                 &
                 its,ite, jts,jte, kts,kte-1                                )

        case (CBMZ_MOSAIC_4BIN,CBMZ_MOSAIC_8BIN,CBMZ_MOSAIC_4BIN_AQ,CBMZ_MOSAIC_8BIN_AQ, &
              CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ,       &
              CBMZ_MOSAIC_DMS_8BIN_AQ, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP)
           call sum_pm_mosaic (                                             &
                grid%alt, chem,                                                  &
                grid%pm2_5_dry, grid%pm2_5_water, grid%pm2_5_dry_ec, grid%pm10,                 &
                ids,ide, jds,jde, kds,kde,                                  &
                ims,ime, jms,jme, kms,kme,                                  &
                its,ite, jts,jte, kts,kte-1                                 )

        case default
           do j=jts,min(jte,jde-1)
              do k=kts,min(kte,kde-1)
                 do i=its,min(ite,ide-1)
                    grid%pm2_5_dry(i,k,j)    = 0.
                    grid%pm2_5_water(i,k,j)  = 0.
                    grid%pm2_5_dry_ec(i,k,j) = 0.
                    grid%pm10(i,k,j)         = 0.
                    grid%tsoa(i,k,j)         = 0.
                 enddo
              enddo
           enddo
        end select
        
        ! initialize advective tendency diagnostics for chem
        if ( grid%itimestep .eq. 0 .and. config_flags%chemdiag .eq. USECHEMDIAG ) then
          grid%advh_ct(:,:,:,:) = 0.
          grid%advz_ct(:,:,:,:) = 0.
        endif
#endif

        ! initialize advective tendency diagnostics for non-chem
        if ( grid%itimestep .eq. 0 .and. config_flags%tenddiag .eq. USETENDDIAG ) then
          advh_t(:,:,:,:) = 0.
          advz_t(:,:,:,:) = 0.
        endif

   IF (num_chem >= PARAM_FIRST_SCALAR ) THEN
! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_c   : DO loop = PARAM_FIRST_SCALAR , num_chem
         CALL set_physical_bc3d( chem(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_c

   ENDIF

   CALL set_physical_bc2d( grid%msftx , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfty , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfux , 'x' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfuy , 'x' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfvx , 'y' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%msfvy , 'y' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%sina , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%cosa , 'r' , config_flags ,              &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%e , 'r' , config_flags ,                 &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )
   CALL set_physical_bc2d( grid%f , 'r' , config_flags ,                 &
                         ids , ide , jds , jde , &
                         ims , ime , jms , jme , &
                         its , ite , jts , jte , &
                         its , ite , jts , jte   )

#if (WRF_CHEM != 1)
      DEALLOCATE(CLDFRA_OLD)
#endif
#ifdef DM_PARALLEL
# include "HALO_EM_INIT_1.inc"
# include "HALO_EM_INIT_2.inc"
# include "HALO_EM_INIT_3.inc"
# include "HALO_EM_INIT_4.inc"
# include "HALO_EM_INIT_5.inc"
# include "PERIOD_BDY_EM_INIT.inc"
# include "PERIOD_BDY_EM_MOIST.inc"
# include "PERIOD_BDY_EM_TKE.inc"
# include "PERIOD_BDY_EM_SCALAR.inc"
# include "PERIOD_BDY_EM_CHEM.inc"
#endif

DEALLOCATE(z_at_q)

   IF (config_flags%p_lev_diags == PRESS_DIAGS ) THEN
    CALL wrf_debug ( 200 , ' PLD: pressure level diags' )
    CALL pld (                                  &
                 !  Input data for computing
                  U=grid%u_2                    &
                 ,V=grid%v_2                    &
                 ,W=grid%w_2                    &
                 ,t=grid%t_2                    &
                 ,qv=moist(:,:,:,P_QV)          &
                 ,zp=grid%ph_2                  &
                 ,zb=grid%phb                   &
                 ,pp=grid%p                     &
                 ,pb=grid%pb                    &
                 ,p=grid%p_hyd                  &
                 ,pw=grid%p_hyd_w               &
                 !  Map factors, coriolis for diags
                 ,msfux=grid%msfux              &
                 ,msfuy=grid%msfuy              &
                 ,msfvx=grid%msfvx              &
                 ,msfvy=grid%msfvy              &
                 ,msftx=grid%msftx              &
                 ,msfty=grid%msfty              &
                 ,f=grid%f                      &
                 ,e=grid%e                      &
                 !  Namelist info
                 ,use_tot_or_hyd_p=config_flags%use_tot_or_hyd_p &
                 ,extrap_below_grnd=config_flags%extrap_below_grnd &
                 ,missing=config_flags%p_lev_missing &
                 !  The diagnostics, mostly output variables
                 ,num_press_levels=config_flags%num_press_levels &
                 ,max_press_levels=max_plevs    &
                 ,press_levels=model_config_rec%press_levels &
                 ,p_pl  = grid%p_pl             &  
                 ,u_pl  = grid%u_pl             &  
                 ,v_pl  = grid%v_pl             &  
                 ,t_pl  = grid%t_pl             &  
                 ,rh_pl = grid%rh_pl            &
                 ,ght_pl= grid%ght_pl           &
                 ,s_pl  = grid%s_pl             &  
                 ,td_pl = grid%td_pl            &
                 ,q_pl = grid%q_pl              &
                 !  Dimension arguments
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde    &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme    &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte    )
   ENDIF

   IF (config_flags%z_lev_diags == Z_DIAGS ) THEN
     CALL wrf_debug ( 200 , ' ZLD: height level and AGL diags' )
     CALL zld (                                  &
                 !  Input data for computing
                  U=grid%u_2                    &
                 ,V=grid%v_2                    &
                 ,W=grid%w_2                    &
                 ,t=grid%t_2                    &
                 ,qv=moist(:,:,:,P_QV)          &
                 ,zp=grid%ph_2                  &
                 ,zb=grid%phb                   &
                 ,pp=grid%p                     &
                 ,pb=grid%pb                    &
                 ,p=grid%p_hyd                  &
                 ,pw=grid%p_hyd_w               &
                 !  Map factors, coriolis for diags
                 ,msfux=grid%msfux              &
                 ,msfuy=grid%msfuy              &
                 ,msfvx=grid%msfvx              &
                 ,msfvy=grid%msfvy              &
                 ,msftx=grid%msftx              &
                 ,msfty=grid%msfty              &
                 ,f=grid%f                      &
                 ,e=grid%e                      &
                 ,ht=grid%ht                    &
                 !  Namelist info
                 ,use_tot_or_hyd_p=config_flags%use_tot_or_hyd_p &
                 ,extrap_below_grnd=config_flags%extrap_below_grnd &
                 ,missing=config_flags%z_lev_missing &
                 !  The diagnostics, mostly output variables
                 ,num_z_levels=config_flags%num_z_levels &
                 ,max_z_levels=max_zlevs    &
                 ,z_levels=model_config_rec%z_levels &
                 ,z_zl  = grid%z_zl             &  
                 ,u_zl  = grid%u_zl             &  
                 ,v_zl  = grid%v_zl             &  
                 ,t_zl  = grid%t_zl             &  
                 ,rh_zl = grid%rh_zl            &
                 ,ght_zl= grid%ght_zl           &
                 ,s_zl  = grid%s_zl             &  
                 ,td_zl = grid%td_zl            &
                 ,q_zl = grid%q_zl              &
                 !  Dimension arguments
                 ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde    &
                 ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme    &
                 ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte    )
   ENDIF

   IF ( config_flags%diag_nwp2 == DO_TRAD_FIELDS ) THEN
      CALL wrf_debug ( 100 , '--> CALL DIAGNOSTICS PACKAGE: TRAD_FIELDS' )
      DO j = jts, MIN(jte,jde-1)
         DO k = 1, kte-1
            DO i = its, MIN(ite,ide-1)
               dz8w(i,k,j) = (grid%phb(i,k+1,j)+grid%ph_2(i,k+1,j))/g - (grid%phb(i,k,j)+grid%ph_2(i,k,j))/g
               grid%rho(i,k,j) = 1./( grid%al(i,k,j)+grid%alb(i,k,j) ) 
            END DO
         END DO
      END DO
      CALL trad_fields ( &
               !  Input data for computing
               U=grid%u_2                                           &
               ,V=grid%v_2                                          &
               ,W=grid%w_2                                          &
               ,t=grid%t_2                                          &
               ,qv=moist(:,:,:,P_QV)                                &
               ,zp=grid%ph_2                                        &
               ,zb=grid%phb                                         &
               ,pp=grid%p                                           &
               ,pb=grid%pb                                          &
               ,p=grid%p_hyd                                        &
               ,pw=grid%p_hyd_w                                     &
               !  Map factors, coriolis for diags
               ,msfux=grid%msfux                                    &
               ,msfuy=grid%msfuy                                    &
               ,msfvx=grid%msfvx                                    &
               ,msfvy=grid%msfvy                                    &
               ,msftx=grid%msftx                                    &
               ,msfty=grid%msfty                                    &
               ,f=grid%f                                            &
               ,e=grid%e                                            &
               ,sina=grid%sina                                      &
               ,cosa=grid%cosa                                      &
               !  Terrestrial data                
               ,rho=grid%rho                                        &
               ,dz8w=dz8w                                           &
               ,ht=grid%ht                                          &
               !  Namelist info
               ,use_theta_m=config_flags%use_theta_m                &
               ,qc=moist(:,:,:,P_QC)                                &
               ,rainc=grid%rainc                                    &
               ,rainnc=grid%rainnc                                  &
               ,snownc=grid%snownc                                  &
               ,graupelnc=grid%graupelnc                            &
               ,hailnc=grid%hailnc                                  &
               !  The diagnostic output variables
               ,sealevelp=grid%sealevelp                            &
               ,temperature=grid%temperature                        &
               ,pressure=grid%pressure                              &
               ,geoheight=grid%geoheight                            &
               ,umet=grid%umet                                      &
               ,vmet=grid%vmet                                      &
               ,speed=grid%speed                                    &
               ,dir=grid%dir                                        &
               ,psfc=grid%psfc                                      &
               ,rain=grid%rain                                      &
               ,liqrain=grid%liqrain                                &
               ,tpw=grid%tpw                                        &
               ,potential_t=grid%potential_t                        &
               ,rh=grid%rh                                          &
               !  Various indexes for declarations, loop bounds
               ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde   &
               ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme   &
               ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe   &
               ,ITS=its,ITE=ite, JTS=jts,JTE=jte, KTS=kts,KTE=kte   )
   ENDIF


!  FIRE
if(config_flags%ifire.eq.2)then

   call fire_driver_em_init ( grid , config_flags    &
            ,ids,ide, kds,kde, jds,jde                &
            ,ims,ime, kms,kme, jms,jme                &
            ,ips,ipe, kps,kpe, jps,jpe )

   CALL wrf_debug ( 100 , 'start_domain_em: After call to fire_driver_em_init' )
endif

#if ( WRFPLUS != 1 )
if( grid%traj_opt /= no_trajectory ) then
     call trajectory_init( grid, config_flags, &
                           ims,ime, jms,jme, kms,kme )
     CALL wrf_debug ( 100 , 'start_domain_em: After call to trajectory_init' )
endif
#endif


     CALL wrf_debug ( 100 , 'start_domain_em: Returning' )

     RETURN

   END SUBROUTINE start_domain_em

!-------------------------------------------------------------------

   SUBROUTINE rebalance_driver_cycl ( grid )

      USE module_domain, ONLY : domain
      IMPLICIT NONE

      TYPE (domain)          :: grid

      CALL rebalance_cycl( grid &
!
#include "actual_new_args.inc"
!
      )

   END SUBROUTINE rebalance_driver_cycl

!---------------------------------------------------------------------

   SUBROUTINE rebalance_cycl ( grid  &
!
#include "dummy_new_args.inc"
!
                        )
      USE module_domain, ONLY : domain
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec
      USE module_model_constants
      USE module_state_description
      USE module_driver_constants, ONLY: DATA_ORDER_XYZ, DATA_ORDER_YXZ, DATA_ORDER_ZXY, &
                                         DATA_ORDER_ZYX, DATA_ORDER_XZY, DATA_ORDER_YZX, &
                                         DATA_ORDER_XY, DATA_ORDER_YX, model_data_order

#ifdef DM_PARALLEL
      USE module_comm_dm, ONLY : &
                           HALO_EM_INIT_1_sub   &
                          ,HALO_EM_INIT_2_sub   &
                          ,HALO_EM_INIT_3_sub   &
                          ,HALO_EM_INIT_4_sub   &
                          ,HALO_EM_INIT_5_sub   &
                          ,HALO_EM_VINTERP_UV_1_sub
#endif
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
   USE module_dm, ONLY : ntasks_x, ntasks_y, ntasks, mytask, local_communicator
#endif

      IMPLICIT NONE

      TYPE (domain)          :: grid

#include "dummy_new_decl.inc"

      TYPE (grid_config_rec_type)              :: config_flags

      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2, qtot
      REAL :: pfu, pfd, phm
      REAL :: z0, z1, z2, w1, w2

      !  Local domain indices and counters.

      INTEGER :: n_moist

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k, kk, ispe, ktf

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

            ktf=MIN(kte,kde-1)

      DO j=jts,jte
         DO i=its,ite
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

         n_moist = num_moist

      print *,'n_moist,PARAM_FIRST_SCALAR',n_moist,PARAM_FIRST_SCALAR

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

        IF (n_moist >= PARAM_FIRST_SCALAR ) THEN
       
               !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
               !  equation) down from the top to get the pressure perturbation.  First get the pressure
               !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.

               kk = kte - 1
               k=kk+1

               qtot = 0.
               DO ispe=PARAM_FIRST_SCALAR,n_moist
                 qtot = qtot + 0.5*(moist(i,kk,j,ispe)+moist(i,kk,j,ispe))
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2

               grid%p(i,kk,j) = - 0.5*((grid%c1f(k)*grid%Mu_2(i,j))+qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/grid%rdnw(kk)/qvf2
               qvf = 1.+rvovrd*moist(i,kk,j,P_QV)
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf*         &
                      (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)


               !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
               !  inverse density fields (total and perturbation).

        DO kk=kte-2,kts,-1
             k = kk + 1

             qtot = 0.
             DO ispe=PARAM_FIRST_SCALAR,n_moist
               qtot = qtot + 0.5*(  moist(i,kk  ,j,ispe) + moist(i,kk+1,j,ispe) )
             ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2
               grid%p(i,kk,j) = grid%p(i,kk+1,j) - ((grid%c1f(k)*grid%Mu_2(i,j)) +       &
                               qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/qvf2/grid%rdn(kk+1)
               qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                           (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
        ENDDO

               !  This is the hydrostatic equation used in the model after the
               !  small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the
               !  geopotential.

            IF (grid%hypsometric_opt == 1) THEN
                  DO kk  = 2,kte
                     k = kk - 1
                     grid%ph_2(i,kk,j) = grid%ph_2(i,kk-1,j) - &
                                   grid%dnw(kk-1) * ( ((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*grid%al(i,kk-1,j) &
                                 + (grid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,kk-1,j) )
                     grid%ph0(i,kk,j) = grid%ph_2(i,kk,j) + grid%phb(i,kk,j)
                  END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
                ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is
                ! dry pressure.
                ! Note that al*p approximates Rd*T and dLOG(p) does z.
                ! Here T varies mostly linear with z, the first-order
                ! integration produces better result.

                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kte
                     pfu = grid%c3f(k  )*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4f(k  ) + grid%p_top
                     pfd = grid%c3f(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4f(k-1) + grid%p_top
                     phm = grid%c3h(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4h(k-1) + grid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO
                  DO k = 1,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO
               DO k = 1,kte
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO
            END IF ! hypsometric option
       ELSE  ! n_moist
         kk = kte - 1
               k = kk + 1
               qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               grid%p(i,kk,j) = - 0.5*((grid%c1f(k)*grid%Mu_2(i,j))+qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/grid%rdnw(kk)/qvf2
               qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf    &
                           *(((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
            !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
            !  inverse density fields (total and perturbation).
           DO kk=kte-2,kts,-1
               k = kk + 1
               qvf1 = 0.5*(moist(i,kk,j,P_QV)+moist(i,kk+1,j,P_QV))
               qvf2 = 1./(1.+qvf1)
               qvf1 = qvf1*qvf2
               grid%p(i,kk,j) = grid%p(i,kk+1,j) - ((grid%c1f(k)*grid%Mu_2(i,j)) + qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/qvf2/grid%rdn(kk+1)
               qvf = 1. + rvovrd*moist(i,kk,j,P_QV)
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                           (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
           ENDDO
               !  This is the hydrostatic equation used in the model after the small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the geopotential.
            IF (grid%hypsometric_opt == 1) THEN
               DO k  = 2,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) - &
                                grid%dnw(k-1) * ( ((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*grid%al(i,k-1,j) &
                              + (grid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,k-1,j) )
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO
            ELSE IF (grid%hypsometric_opt == 2) THEN
             ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is dry
             ! pressure.
             ! Note that al*p approximates Rd*T and dLOG(p) does z.
             ! Here T varies mostly linear with z, the first-order integration
             ! produces better result.
               grid%ph_2(i,1,j) = grid%phb(i,1,j)
               DO k = 2,kte
                  pfu = grid%c3f(k  )*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4f(k  ) + grid%p_top
                  pfd = grid%c3f(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4f(k-1) + grid%p_top
                  phm = grid%c3h(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4h(k-1) + grid%p_top
                  grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO

               DO k = 1,kte
                  grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
               END DO

               DO k = 1,kte
                  grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
               END DO

            END IF ! hypsometric
       ENDIF ! nmoist

! update surface pressure PSFC (needed for post-processing):
               z0 = grid%ph0(i,1,j)/g
               z1 = 0.5*(grid%ph0(i,1,j)+grid%ph0(i,2,j))/g
               z2 = 0.5*(grid%ph0(i,2,j)+grid%ph0(i,3,j))/g
               w1 = (z0 - z2)/(z1 - z2)
               w2 = 1. - w1
               grid%psfc(i,j) = w1*(grid%p(i,1,j)+grid%pb(i,1,j))+w2*(grid%p(i,2,j)+grid%pb(i,2,j))

         END DO !i
        ENDDO !j

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "HALO_EM_INIT_1.inc"
# include "HALO_EM_INIT_2.inc"
# include "HALO_EM_INIT_3.inc"
# include "HALO_EM_INIT_4.inc"
# include "HALO_EM_INIT_5.inc"
#endif
   END SUBROUTINE rebalance_cycl

!---------------------------------------------------------------------
