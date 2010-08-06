!-------------------------------------------------------------------

   SUBROUTINE start_domain_em ( grid, allowed_to_read &
! Actual arguments generated from Registry
# include "dummy_new_args.inc"
!
)

   USE module_domain, ONLY : domain, wrfu_timeinterval, get_ijk_from_grid, &
        domain_setgmtetc
   USE module_state_description
   USE module_model_constants
   USE module_bc, ONLY : boundary_condition_check, set_physical_bc2d
   USE module_bc_em
   USE module_configure, ONLY : grid_config_rec_type
   USE module_tiles, ONLY : set_tiles
#ifdef DM_PARALLEL
   USE module_dm, ONLY : wrf_dm_min_real, wrf_dm_maxval, ntasks_x, ntasks_y, &
        local_communicator_periodic, local_communicator, mytask, ntasks 
#else
   USE module_dm, ONLY : wrf_dm_min_real
#endif
   USE module_comm_dm

   USE module_physics_init
   USE module_fr_sfire_driver, ONLY : sfire_driver_em_init
#ifdef WRF_CHEM
   USE module_aerosols_sorgam, ONLY: sum_pm_sorgam
   USE module_gocart_aerosols, ONLY: sum_pm_gocart
   USE module_mosaic_driver, ONLY: sum_pm_mosaic
   USE module_input_tracer, ONLY: initialize_tracer
#endif

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

   REAL        :: p00, t00, a, tiso, p_surf, pd_surf, temp
#ifdef WRF_CHEM
   REAL        RGASUNIV ! universal gas constant [ J/mol-K ]
   PARAMETER ( RGASUNIV = 8.314510 )
      REAL,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: &
                      z_at_w,convfac
   REAL :: tempfac
#endif

   REAL :: qvf1, qvf2, qvf
   REAL :: MPDT
   REAL :: spongeweight
   LOGICAL :: first_trip_for_this_domain, start_of_simulation, fill_w_flag
#ifndef WRF_CHEM
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: cldfra_old
#endif

   REAL :: lat1 , lat2 , lat3 , lat4
   REAL :: lon1 , lon2 , lon3 , lon4
   INTEGER :: num_points_lat_lon , iloc , jloc
   CHARACTER (LEN=132) :: message
   TYPE(WRFU_TimeInterval) :: stepTime
   REAL, DIMENSION(:,:), ALLOCATABLE :: clat_glob
   logical :: f_flux  ! flag for computing averaged fluxes in cu_gd

   INTEGER :: idex, jdex
   INTEGER :: im1,ip1,jm1,jp1
   REAL :: hx,hy,pi

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
#ifndef WRF_CHEM
         ALLOCATE(CLDFRA_OLD(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_OLD = 0.
#endif
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

   IF ( ( MOD (ide-ids,config_flags%parent_grid_ratio) .NE. 0 ) .OR. &
        ( MOD (jde-jds,config_flags%parent_grid_ratio) .NE. 0 ) ) THEN
      WRITE(message, FMT='("Nested dimensions are illegal for domain ",I2,":  Both &
         &MOD(",I4,"-",I1,",",I2,") and MOD(",I4,"-",I1,",",I2,") must = 0" )') &
         grid%id,ide,ids,config_flags%parent_grid_ratio,jde,jds,config_flags%parent_grid_ratio
      CALL wrf_error_fatal ( message )
   END IF

   IF ( config_flags%polar ) THEN
!write(0,*)__FILE__,__LINE__,' clat ',ips,ipe,jps,jpe
!do j = jps,jpe
!write(0,*)__FILE__,__LINE__,' clat ',ids,j,grid%clat(ips,j)
!enddo

#ifdef DM_PARALLEL
! WARNING:  this might present scaling issues on very large numbers of processors
     ALLOCATE( clat_glob(ids:ide,jds:jde) )

     CALL wrf_patch_to_global_real ( grid%clat, clat_glob, grid%domdesc, 'xy', 'xy', &
                                     ids, ide, jds, jde, 1, 1, &
                                     ims, ime, jms, jme, 1, 1, &
                                     its, ite, jts, jte, 1, 1 )

     CALL wrf_dm_bcast_real ( clat_glob , (ide-ids+1)*(jde-jds+1) )

     grid%clat_xxx(ipsx:ipex,jpsx:jpex) = clat_glob(ipsx:ipex,jpsx:jpex)

     DEALLOCATE( clat_glob )
#endif
   ENDIF

! here we check to see if the boundary conditions are set properly

   CALL boundary_condition_check( config_flags, bdyzone, error, grid%id )

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

   IF ( config_flags%restart .or. grid%moved ) THEN
       first_trip_for_this_domain = .TRUE.
   ENDIF

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

   IF ( .NOT. config_flags%restart .AND. &
        (( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ))) THEN

      IF ( config_flags%map_proj .EQ. 0 ) THEN
         CALL wrf_error_fatal ( 'start_domain: Idealized case cannot have a separate nested input file' )
      END IF

      IF ( config_flags%use_baseparam_fr_nml ) then
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
      CALL nl_get_iso_temp   ( 1 , tiso )

      ELSE
      ! get these constants from model data

      t00  = grid%t00
      p00  = grid%p00
      a    = grid%tlp
      tiso = grid%tiso

      IF (t00 .LT. 100. .or. p00 .LT. 10000.) THEN
      WRITE(wrf_err_message,*)&
      'start_em: did not find base state parameters in wrfinput. Add use_baseparam_fr_nml = .t. in &dynamics and rerun'
      CALL wrf_error_fatal(TRIM(wrf_err_message))
      ENDIF

      ENDIF

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

            !  Base state pressure is a function of eta level and terrain, only, plus
            !  the hand full of constants: p00 (sea level pressure, Pa), t00 (sea level
            !  temperature, K), and A (temperature difference, from 1000 mb to 300 mb, K).

            p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )

            DO k = 1, kte-1
               grid%pb(i,k,j) = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
               grid%t_init(i,k,j) = temp*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
!              grid%t_init(i,k,j) = (t00 + A*LOG(grid%pb(i,k,j)/p00))*(p00/grid%pb(i,k,j))**(r_d/cp) - t0
               grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
            END DO

            !  Base state mu is defined as base state surface pressure minus grid%p_top

            grid%mub(i,j) = p_surf - grid%p_top

            !  Integrate base geopotential, starting at terrain elevation.  This assures that
            !  the base state is in exact hydrostatic balance with respect to the model equations.
            !  This field is on full levels.

            grid%phb(i,1,j) = grid%ht(i,j) * g
            DO k  = 2,kte
               grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
            END DO
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
       DO i = its, min(ite,ide-1)
           IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
             grid%mu_1(i,j)=grid%mu_2(i,j)
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
         grid%pb(i,k,j) = grid%znu(k)*grid%mub(i,j)+grid%p_top
         grid%alb(i,k,j) = (r_d/p1000mb)*(grid%t_init(i,k,j)+t0)*(grid%pb(i,k,j)/p1000mb)**cvpm
       ENDIF
     ENDDO
     ENDDO
     ENDDO
    ELSE
! with nests, grid%t_init generally needs recomputations (since it is not interpolated)
     DO j = jts,min(jte,jde-1)
     DO k = kts,kte-1
     DO i = its, min(ite,ide-1)
       IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
         grid%pb(i,k,j) = grid%znu(k)*grid%mub(i,j)+grid%p_top
         grid%alb(i,k,j) = -grid%rdnw(k)*(grid%phb(i,k+1,j)-grid%phb(i,k,j))/grid%mub(i,j)
         grid%t_init(i,k,j) = grid%alb(i,k,j)*(p1000mb/r_d)/((grid%pb(i,k,j)/p1000mb)**cvpm) - t0
       ENDIF
     ENDDO
     ENDDO
     ENDDO
    ENDIF

     DO j = jts,min(jte,jde-1)

       k = kte-1
       DO i = its, min(ite,ide-1)
         IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
           qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k,j,P_QV))
           qvf2 = 1./(1.+qvf1)
           qvf1 = qvf1*qvf2
           grid%p(i,k,j) = - 0.5*(grid%mu_1(i,j)+qvf1*grid%mub(i,j))/grid%rdnw(k)/qvf2
           grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
           qvf = 1. + rvovrd*moist(i,k,j,P_QV)
           grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf*(((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
           grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
         ENDIF
       ENDDO

       DO k = kte-2, 1, -1
       DO i = its, min(ite,ide-1)
         IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
           qvf1 = 0.5*(moist(i,k,j,P_QV)+moist(i,k+1,j,P_QV))
           qvf2 = 1./(1.+qvf1)
           qvf1 = qvf1*qvf2
           grid%p(i,k,j) = grid%p(i,k+1,j) - (grid%mu_1(i,j) + qvf1*grid%mub(i,j))/qvf2/grid%rdn(k+1)
           grid%p_hyd(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
           qvf = 1. + rvovrd*moist(i,k,j,P_QV)
           grid%alt(i,k,j) = (r_d/p1000mb)*(grid%t_1(i,k,j)+t0)*qvf* &
                        (((grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb)**cvpm)
           grid%al(i,k,j) = grid%alt(i,k,j) - grid%alb(i,k,j)
         ENDIF
       ENDDO
       ENDDO

     ENDDO

   ENDIF

   IF ( grid%press_adj .and. ( grid%id .NE. 1 ) .AND. .NOT. ( config_flags%restart ) .AND. &
       ( ( config_flags%input_from_hires ) .OR. ( config_flags%input_from_file ) ) ) THEN
      DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)
            grid%mu_2(i,j) = grid%mu_2(i,j) + grid%al(i,1,j) / ( grid%alt(i,1,j) * grid%alb(i,1,j) ) * &
                                    g * ( grid%ht(i,j) - grid%ht_fine(i,j) )
         END DO
       END DO
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
          grid%mu_1(i,j)=grid%mu_2(i,j)
       ENDDO
       ENDDO

   END IF

   IF ( first_trip_for_this_domain ) THEN

   CALL wrf_debug ( 100 , 'start_domain_em: Before call to phy_init' )

! namelist MPDT does not exist yet, so set it here
! MPDT is the call frequency for microphysics in minutes (0 means every step)
   MPDT = 0.

! set GMT outside of phy_init because phy_init may not be called on this
! process if, for example, it is a moving nest and if this part of the domain is not
! being initialized (not the leading edge).
   CALL domain_setgmtetc( grid, start_of_simulation )

!-----------------------------------------------------------------------------
! Adaptive time step: Added by T. Hutchinson, WSI  11/6/07
!
!

   IF ( ( grid%use_adaptive_time_step ) .AND. &
        ( ( grid%dfi_opt .EQ. DFI_NODFI ) .OR. ( grid%dfi_stage .EQ. DFI_FST ) ) ) THEN

!  Calculate any variables that were not set

      if (grid%starting_time_step == -1) then
         grid%starting_time_step = NINT(6 * MIN(grid%dx,grid%dy) / 1000)
      endif

      if (grid%max_time_step == -1) then
         grid%max_time_step = 3*grid%starting_time_step
      endif

      if (grid%min_time_step == -1) then
         grid%min_time_step = 0.5*grid%starting_time_step
      endif

!     Set a starting timestep.

      grid%dt = grid%starting_time_step / grid%parent_time_step_ratio

!     Check to assure that time_step_sound is to be dynamically set.

      CALL nl_set_time_step_sound ( 1 , 0 )
      grid%time_step_sound = 0

      grid%max_msftx=MAXVAL(grid%msftx)
      grid%max_msfty=MAXVAL(grid%msfty)
#ifdef DM_PARALLEL
      CALL wrf_dm_maxval(grid%max_msftx, idex, jdex)
      CALL wrf_dm_maxval(grid%max_msfty, idex, jdex)
#endif

!     This first call just initializes variables.
!     If a restart, get initialized variables from restart file

      IF ( .NOT. ( config_flags%restart ) ) then
         CALL adapt_timestep(grid, config_flags)
      END IF


   END IF

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
   DO ij = 1, grid%num_tiles

!tgs do not need physics initialization for backward DFI integration
     IF ( grid%dfi_opt .NE. DFI_NODFI .and. grid%dfi_stage .EQ. DFI_FST) THEN     !tgs
        grid%stepra=nint(grid%RADT*60./grid%DT)
        grid%stepra=max(grid%stepra,1)
        grid%stepbl=nint(grid%BLDT*60./grid%DT)
        grid%stepbl=max(grid%stepbl,1)
        grid%stepcu=nint(grid%CUDT*60./grid%DT)
        grid%stepcu=max(grid%stepcu,1)
        grid%stepfg=nint(grid%FGDT*60./grid%DT)
        grid%stepfg=max(grid%stepfg,1)
     ENDIF
     
     IF ( ( grid%dfi_opt .NE. DFI_NODFI .and. grid%dfi_stage .EQ. DFI_FWD) .or.            &
          ( grid%dfi_opt .EQ. DFI_NODFI)) THEN

     CALL phy_init (  grid%id , config_flags, grid%DT, grid%RESTART, grid%znw, grid%znu,   &
                      grid%p_top, grid%tsk, grid%RADT,grid%BLDT,grid%CUDT, MPDT, &
                      grid%rthcuten, grid%rqvcuten, grid%rqrcuten,           &
                      grid%rqccuten, grid%rqscuten, grid%rqicuten,           &
                      grid%rublten,grid%rvblten,grid%rthblten,               &
                      grid%rqvblten,grid%rqcblten,grid%rqiblten,             &
                      grid%rthraten,grid%rthratenlw,grid%rthratensw,         &
                      grid%stepbl,grid%stepra,grid%stepcu,                   &
                      grid%w0avg, grid%rainnc, grid%rainc, grid%raincv, grid%rainncv,  &
                      grid%nca,grid%swrad_scat,                    &
                      grid%cldefi,grid%lowlyr,                          &
                      grid%mass_flux,                              &
                      grid%rthften, grid%rqvften,                       &
                      grid%cldfra,                                      &
#ifdef WRF_CHEM
                      grid%cldfra_old,                                  &
#endif
#ifndef WRF_CHEM
                      cldfra_old,                                  &
#endif
                      grid%glw,grid%gsw,grid%emiss,grid%embck,            &
                      grid%lu_index,                                      &
                      grid%landuse_ISICE, grid%landuse_LUCATS,            &
                      grid%landuse_LUSEAS, grid%landuse_ISN,              &
                      grid%lu_state,                                      &
                      grid%xlat,grid%xlong,grid%albedo,grid%albbck,grid%GMT,grid%JULYR,grid%JULDAY,     &
                      grid%levsiz, num_ozmixm, num_aerosolc, grid%paerlev,  &
                      grid%tmn,grid%xland,grid%znt,grid%z0,grid%ust,grid%mol,grid%pblh,grid%tke_myj,    &
                      grid%tke_pbl,                                                                  &
                      grid%exch_h,grid%thc,grid%snowc,grid%mavail,grid%hfx,grid%qfx,grid%rainbl, &
                      grid%tslb,grid%zs,grid%dzs,config_flags%num_soil_layers,grid%warm_rain,  &
                      grid%adv_moist_cond,                         &
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
                      config_flags%num_urban_layers,                        & !multi-layer urban
                      ozmixm,grid%pin,                             &     ! Optional
                      grid%m_ps_1,grid%m_ps_2,grid%m_hybi,aerosolc_1,aerosolc_2,&  ! Optional
                      grid%rundgdten,grid%rvndgdten,grid%rthndgdten,         &     ! Optional
                      grid%rphndgdten,grid%rqvndgdten,grid%rmundgdten,       &     ! Optional
                      grid%FGDT,grid%stepfg,                        &     ! Optional
                      grid%cugd_tten,grid%cugd_ttens,grid%cugd_qvten,        &   ! Optional
                      grid%cugd_qvtens,grid%cugd_qcten,                 &   ! Optional
                      grid%DZR, grid%DZB, grid%DZG,                          & !Optional urban
                      grid%TR_URB2D,grid%TB_URB2D,grid%TG_URB2D,grid%TC_URB2D,    & !Optional urban
                      grid%QC_URB2D, grid%XXXR_URB2D,grid%XXXB_URB2D,        & !Optional urban
                      grid%XXXG_URB2D, grid%XXXC_URB2D,                 & !Optional urban
                      grid%TRL_URB3D, grid%TBL_URB3D, grid%TGL_URB3D,        & !Optional urban
                      grid%SH_URB2D, grid%LH_URB2D, grid%G_URB2D, grid%RN_URB2D,  & !Optional urban
                      grid%TS_URB2D, grid%FRC_URB2D, grid%UTYPE_URB2D,      & !Optional urban
                      grid%TRB_URB4D,grid%TW1_URB4D,grid%TW2_URB4D,grid%TGB_URB4D,grid%TLEV_URB3D,  & !multi-layer urban
                      grid%QLEV_URB3D,grid%TW1LEV_URB3D,grid%TW2LEV_URB3D,        & !multi-layer urban
                      grid%TGLEV_URB3D,grid%TFLEV_URB3D,grid%SF_AC_URB3D,         & !multi-layer urban 
                      grid%LF_AC_URB3D,grid%CM_AC_URB3D,grid%SFVENT_URB3D,grid%LFVENT_URB3D,   & !multi-layer urban
                      grid%SFWIN1_URB3D,grid%SFWIN2_URB3D, & !multi-layer urban
                      grid%SFW1_URB3D,grid%SFW2_URB3D,grid%SFR_URB3D,grid%SFG_URB3D,     & !multi-layer urban
                      grid%A_U_BEP,grid%A_V_BEP,grid%A_T_BEP,grid%A_Q_BEP,             & !multi-layer urban
                      grid%A_E_BEP,grid%B_U_BEP,grid%B_V_BEP,grid%B_T_BEP,             & !multi-layer urban
                      grid%B_Q_BEP,grid%B_E_BEP,grid%DLG_BEP,                          & !multi-layer urban
                      grid%DL_U_BEP,grid%SF_BEP,grid%VL_BEP,                           & !multi-layer urban
                      grid%TML,grid%T0ML,grid%HML,grid%H0ML,grid%HUML,grid%HVML,grid%TMOML,     & !Optional oml
                      grid%itimestep, grid%fdob,            &
                      t00, p00, a,                      & ! for obs_nudge base state
                      grid%TYR, grid%TYRA, grid%TDLY, grid%TLAG, grid%NYEAR, grid%NDAY,grid%tmn_update, &
                      grid%achfx, grid%aclhf, grid%acgrdflx                 &
                      )
       ENDIF   !tgs

   ENDDO

   CALL wrf_debug ( 100 , 'start_domain_em: After call to phy_init' )

   IF (config_flags%do_avgflx_em .EQ. 1) THEN
      WRITE ( message , FMT = '("start_em: initializing avgflx on domain ",I3)' ) &
           & grid%id
      CALL wrf_message(trim(message)) 
      grid%avgflx_count = 0
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

   END IF

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
#  include "HALO_EM_INIT_1.inc"
#  include "HALO_EM_INIT_2.inc"
#  include "HALO_EM_INIT_3.inc"
#  include "HALO_EM_INIT_4.inc"
#  include "HALO_EM_INIT_5.inc"
#  include "PERIOD_BDY_EM_INIT.inc"
#  include "PERIOD_BDY_EM_MOIST.inc"
#  include "PERIOD_BDY_EM_TKE.inc"
#  include "PERIOD_BDY_EM_SCALAR.inc"
#  include "PERIOD_BDY_EM_CHEM.inc"
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

! finished setting kinematic condition for w at the surface

! set up slope-radiation constant arrays based on topography
       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
          im1 = max(i-1,ids)
          ip1 = min(i+1,ide-1)
          jm1 = max(j-1,jds)
          jp1 = min(j+1,jde-1)
          grid%toposlpx(i,j)=(grid%ht(ip1,j)-grid%ht(im1,j))*grid%msftx(i,j)*grid%rdx/(ip1-im1)
          grid%toposlpy(i,j)=(grid%ht(i,jp1)-grid%ht(i,jm1))*grid%msfty(i,j)*grid%rdy/(jp1-jm1)
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


#ifdef WRF_CHEM
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
                         grid%gd_cloud, grid%gd_cloud2,grid%raincv_a,grid%raincv_b,     &
                         grid%gd_cloud_a, grid%gd_cloud2_a,                              &
                         grid%gd_cloud_b, grid%gd_cloud2_b,                              &
                         grid%tauaer1,grid%tauaer2,grid%tauaer3,grid%tauaer4,                   &
                         grid%gaer1,grid%gaer2,grid%gaer3,grid%gaer4,                           &
                         grid%waer1,grid%waer2,grid%waer3,grid%waer4,                           &
                         grid%l2aer,grid%l3aer,grid%l4aer,grid%l5aer,grid%l6aer,grid%l7aer,     &
                         grid%pm2_5_dry,grid%pm2_5_water,grid%pm2_5_dry_ec,                &
                         grid%last_chem_time_year,grid%last_chem_time_month,               &
                         grid%last_chem_time_day,grid%last_chem_time_hour,                 &
                         grid%last_chem_time_minute,grid%last_chem_time_second,            &
                         grid%chem_in_opt,grid%kemit,                       &
                         ids , ide , jds , jde , kds , kde ,                &
                         ims , ime , jms , jme , kms , kme ,                &
                         its , ite , jts , jte , kts , kte                  )

!
! calculate initial pm
!
!       print *,'calculating initial pm'
        select case (config_flags%chem_opt)
        case (GOCART_SIMPLE,GOCARTRACM_KPP,GOCARTRADM2,GOCARTRADM2_KPP)
           call sum_pm_gocart (                                             &
                grid%alt, chem, grid%pm2_5_dry, grid%pm2_5_dry_ec,grid%pm10,&
                ids,ide, jds,jde, kds,kde,                                  &
                ims,ime, jms,jme, kms,kme,                                  &
                its,ite, jts,jte, kts,kte-1                                 )
        case (RADM2SORG,RADM2SORG_AQ,RADM2SORG_KPP,RACMSORG_AQ,RACMSORG_KPP)
           call sum_pm_sorgam (                                             &
                grid%alt, chem, grid%h2oaj, grid%h2oai,                                    &
                grid%pm2_5_dry, grid%pm2_5_water, grid%pm2_5_dry_ec, grid%pm10,                 &
                ids,ide, jds,jde, kds,kde,                                  &
                ims,ime, jms,jme, kms,kme,                                  &
                its,ite, jts,jte, kts,kte-1                                 )

        case (CBMZ_MOSAIC_4BIN,CBMZ_MOSAIC_8BIN,CBMZ_MOSAIC_4BIN_AQ,CBMZ_MOSAIC_8BIN_AQ)
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
                 enddo
              enddo
           enddo
        end select
#endif

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

#ifndef WRF_CHEM
      DEALLOCATE(CLDFRA_OLD)
#endif
#ifdef DM_PARALLEL
#   include "HALO_EM_INIT_1.inc"
#   include "HALO_EM_INIT_2.inc"
#   include "HALO_EM_INIT_3.inc"
#   include "HALO_EM_INIT_4.inc"
#   include "HALO_EM_INIT_5.inc"
#  include "PERIOD_BDY_EM_INIT.inc"
#  include "PERIOD_BDY_EM_MOIST.inc"
#  include "PERIOD_BDY_EM_TKE.inc"
#  include "PERIOD_BDY_EM_SCALAR.inc"
#  include "PERIOD_BDY_EM_CHEM.inc"
#endif

!  FIRE
if(config_flags%ifire.eq.2)then

   call sfire_driver_em_init ( grid , config_flags    &
            ,ids,ide, kds,kde, jds,jde                &
            ,ims,ime, kms,kme, jms,jme                &
            ,ips,ipe, kps,kpe, jps,jpe ) 

   CALL wrf_debug ( 100 , 'start_domain_em: After call to sfire_driver_em_init' )
endif


     CALL wrf_debug ( 100 , 'start_domain_em: Returning' )

     RETURN

   END SUBROUTINE start_domain_em

