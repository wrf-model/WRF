!-------------------------------------------------------------------

   SUBROUTINE a_start_domain_em ( grid, allowed_to_read &
! Actual arguments generated from Registry
# include "dummy_new_args.inc"
!
)

   USE module_domain, ONLY : domain, wrfu_timeinterval, get_ijk_from_grid, &
        domain_setgmtetc
   USE module_state_description
   USE module_model_constants
   USE a_module_bc, ONLY : a_set_physical_bc2d
   USE a_module_bc_em
   USE module_configure, ONLY : grid_config_rec_type
   USE module_tiles, ONLY : set_tiles
#ifdef DM_PARALLEL
   USE module_dm, ONLY : wrf_dm_min_real, wrf_dm_max_real, wrf_dm_maxval, &
        ntasks_x, ntasks_y, &
        local_communicator_periodic, local_communicator, mytask, ntasks 
#else
   USE module_dm, ONLY : wrf_dm_min_real,  wrf_dm_max_real
#endif
   USE module_comm_dm

   USE a_module_physics_init
   USE module_diag_pld, ONLY : pld

   USE module_model_constants

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

   REAL        :: p00, t00, a, tiso, p_surf, pd_surf, temp, tiso_tmp
   REAL        :: p_strat, a_strat
   REAL :: qvf1, qvf2, qvf, a_qvf
   REAL :: pfu, pfd, phm, a_pfu, a_pfd, a_phm
   REAL :: MPDT
   REAL :: spongeweight
   LOGICAL :: first_trip_for_this_domain, start_of_simulation, fill_w_flag
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

#if (WRF_CHEM!=1)
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
   REAL :: temp2, temp1, temp0, temp0b, tempb, tempb0 
   CHARACTER (LEN=256) :: timestr

   REAL :: w_max, w_min
   LOGICAL :: w_needs_to_be_set

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
#if (WRF_CHEM!=1)
         ALLOCATE(CLDFRA_OLD(IMS:IME,KMS:KME,JMS:JME),STAT=I)  ; CLDFRA_OLD = 0.
#endif
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

!kludge - need to stop CG from resetting precip and phys tendencies to zero
!         when we are in here due to a nest being spawned, we want to still
!         recompute the base state, but that is about it
   !  This is temporary and will need to be changed when grid%itimestep is removed.

   IF ( grid%itimestep .EQ. 0 ) THEN
      first_trip_for_this_domain = .TRUE.
   ELSE
      first_trip_for_this_domain = .FALSE.
   END IF

   IF ( config_flags%use_baseparam_fr_nml ) then
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
      CALL nl_get_iso_temp   ( 1 , tiso )
      CALL nl_get_base_lapse_strat ( 1 , a_strat )
      CALL nl_get_base_pres_strat  ( 1 , p_strat )
      IF ( ( t00 .LT. 100. .or. p00 .LT. 10000.) .AND. ( .NOT. grid%this_is_an_ideal_run ) ) THEN
         WRITE(wrf_err_message,*) 'start_em: BAD BASE STATE for T00 or P00 in namelist.input file'
         CALL wrf_error_fatal(TRIM(wrf_err_message))
      END IF

   ELSE
   ! get these constants from model data

      t00  = grid%t00
      p00  = grid%p00
      a    = grid%tlp
      tiso = grid%tiso
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
               grid%pb(i,k,j) = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
               temp = MAX ( tiso, t00 + A*LOG(grid%pb(i,k,j)/p00) )
               IF ( grid%pb(i,k,j) .LT. p_strat ) THEN
                  temp = tiso + A_strat * LOG ( grid%pb(i,k,j)/p_strat )
               ENDIF
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

            IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
               DO k  = 2,kte
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) - grid%dnw(k-1)*grid%mub(i,j)*grid%alb(i,k-1,j)
               END DO
            ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
               DO k  = 2,kte
                  pfu = grid%mub(i,j)*grid%znw(k)   + grid%p_top
                  pfd = grid%mub(i,j)*grid%znw(k-1) + grid%p_top
                  phm = grid%mub(i,j)*grid%znu(k-1) + grid%p_top
                  grid%phb(i,k,j) = grid%phb(i,k-1,j) + grid%alb(i,k-1,j)*phm*LOG(pfd/pfu)
               END DO
            END IF
         END DO
      END DO

   ENDIF

#ifdef DM_PARALLEL
#   include "HALO_EM_INIT_5_AD.inc"
#   include "HALO_EM_INIT_4_AD.inc"
#   include "HALO_EM_INIT_3_AD.inc"
#   include "HALO_EM_INIT_2_AD.inc"
#   include "HALO_EM_INIT_1_AD.inc"
#endif

        ! initialize advective tendency diagnostics for non-chem
        if ( grid%itimestep .eq. 0 .and. config_flags%tenddiag .eq. USETENDDIAG ) then
          a_advh_t(:,:,:,:) = 0.
          a_advz_t(:,:,:,:) = 0.
        endif

   IF (num_scalar > 0) THEN

! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_s   : DO loop = 1 , num_scalar
         CALL a_set_physical_bc3d( a_scalar(:,:,:,loop) , 'r' , config_flags ,               &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_s

   ENDIF

   IF (num_moist > 0) THEN

! use of (:,:,:,loop) not efficient on DEC, but (ims,kms,jms,loop) not portable to SGI/Cray

      loop_3d_m   : DO loop = 1 , num_moist
         CALL a_set_physical_bc3d( a_moist(:,:,:,loop) , 'r' , config_flags ,                 &
                                 ids , ide , jds , jde , kds , kde ,        &
                                 ims , ime , jms , jme , kms , kme ,        &
                                 its , ite , jts , jte , kts , kte ,        &
                                 its , ite , jts , jte , kts , kte )
      END DO loop_3d_m

   ENDIF

   CALL a_set_physical_bc3d( grid%a_w_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL a_set_physical_bc3d( grid%a_w_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL a_set_physical_bc3d( grid%a_ph_1 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL a_set_physical_bc3d( grid%a_ph_2 , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL a_set_physical_bc3d( grid%a_t_1 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL a_set_physical_bc3d( grid%a_t_2 , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL a_set_physical_bc2d( grid%a_mu_1, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL a_set_physical_bc2d( grid%a_mu_2, 't' , config_flags ,   &
                           ids , ide , jds , jde ,  &
                           ims , ime , jms , jme ,  &
                           its , ite , jts , jte ,  &
                           its , ite , jts , jte   )
   CALL a_set_physical_bc3d( grid%a_php , 'W' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL a_set_physical_bc3d( grid%a_al , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL a_set_physical_bc3d( grid%a_alt , 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL a_set_physical_bc3d( grid%a_tke_2, 't' , config_flags ,                 &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   IF ( first_trip_for_this_domain ) THEN

! set GMT outside of phy_init because phy_init may not be called on this
! process if, for example, it is a moving nest and if this part of the domain is not
! being initialized (not the leading edge).
   CALL domain_setgmtetc( grid, start_of_simulation )
!tgs
   IF(config_flags%cycling) start_of_simulation = .true.
!  print *,'cycling, start_of_simulation -->',config_flags%cycling, start_of_simulation

! Set the w at the surface.  If this is the start of a forecast, or if this is a cycled run
! and the start of that forecast, we define the w field.  However, if this a restart, then 
! we leave w alone.  Initial value is V dot grad(topo) at surface, then smoothly decaying
! above that.
   END IF

   IF ( ( .NOT. start_of_simulation .OR. config_flags%cycling ) .AND. ( .NOT. config_flags%restart ) ) THEN

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
      ELSE
         IF ( config_flags%use_input_w ) THEN
            w_needs_to_be_set = .FALSE.
         ELSE
            w_needs_to_be_set = .TRUE.
         END IF
      END IF

      IF ( w_needs_to_be_set ) THEN

         fill_w_flag = .true.
         CALL a_set_w_surface(  config_flags, grid%znw, fill_w_flag,             &
                              grid%w_1, grid%a_w_1, grid%ht, &
                              grid%u_1, grid%a_u_1, grid%v_1, grid%a_v_1, grid%cf1, &
                              grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                              ids, ide, jds, jde, kds, kde,                    &
                              ims, ime, jms, jme, kms, kme,                    &
                              its, ite, jts, jte, kts, kte                     )
         CALL a_set_w_surface(  config_flags, grid%znw, fill_w_flag,             &
                              grid%w_2, grid%a_w_2, grid%ht,  &
                              grid%u_2, grid%a_u_2, grid%v_2, grid%a_v_2, grid%cf1, &
                              grid%cf2, grid%cf3, grid%rdx, grid%rdy, grid%msftx, grid%msfty, &
                              ids, ide, jds, jde, kds, kde,                    &
                              ims, ime, jms, jme, kms, kme,                    &
                              its, ite, jts, jte, kts, kte                     ) 
      ENDIF

   ENDIF

! finished setting kinematic condition for w at the surface

   CALL a_set_physical_bc3d( grid%a_u_1 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL a_set_physical_bc3d( grid%a_u_2 , 'U' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

   CALL a_set_physical_bc3d( grid%a_v_1 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )
   CALL a_set_physical_bc3d( grid%a_v_2 , 'V' , config_flags ,                  &
                         ids , ide , jds , jde , kds , kde ,        &
                         ims , ime , jms , jme , kms , kme ,        &
                         its , ite , jts , jte , kts , kte ,        &
                         its , ite , jts , jte , kts , kte )

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
#  include "HALO_EM_INIT_5_AD.inc"
#  include "HALO_EM_INIT_4_AD.inc"
#  include "HALO_EM_INIT_3_AD.inc"
#  include "HALO_EM_INIT_2_AD.inc"
#  include "HALO_EM_INIT_1_AD.inc"
#endif

   IF ( first_trip_for_this_domain ) THEN

   CALL wrf_debug ( 100 , 'start_domain_em_ad: Before call to a_phy_init' )

! namelist MPDT does not exist yet, so set it here
! MPDT is the call frequency for microphysics in minutes (0 means every step)
   MPDT = 0.

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
    IF ( ( grid%dfi_opt .EQ. DFI_NODFI ) .or. &
          ( ( grid%dfi_stage .NE. DFI_BCK ) .and. &
          ( grid%dfi_stage .NE. DFI_STARTBCK ) ) ) THEN    !tgs, mods by tah

     CALL a_phy_init( grid%id , config_flags, grid%DT, grid%RESTART, grid%znw, grid%znu,   &
                      grid%p_top, grid%tsk, grid%RADT,grid%BLDT,grid%CUDT, MPDT, &
                      grid%rucuten, grid%rvcuten, grid%rthcuten, grid%a_rthcuten, &
                      grid%rqvcuten, grid%a_rqvcuten, grid%rqrcuten, grid%rqccuten, &
                      grid%a_rqccuten, &
                      grid%rqscuten, grid%rqicuten,                          &
                      grid%rushten, grid%rvshten, grid%rthshten,             &
                      grid%rqvshten, grid%rqrshten, grid%rqcshten,           &
                      grid%rqsshten, grid%rqishten, grid%rqgshten,           &
                      grid%rublten,grid%a_rublten,grid%rvblten,grid%a_rvblten, &
                      grid%rthblten, grid%a_rthblten,              &
                      grid%rqvblten,grid%a_rqvblten,grid%rqcblten,grid%rqiblten, &
                      grid%rthraten,grid%rthratenlw,grid%rthratensw,         &
                      grid%stepbl,grid%stepra,grid%stepcu,                   &
                      grid%w0avg, grid%a_w0avg, grid%rainnc, grid%a_rainnc, &
                      grid%rainc, grid%a_rainc, grid%raincv, grid%a_raincv, &
                      grid%rainncv, grid%a_rainncv, &
                      grid%snownc, grid%a_snownc, grid%snowncv, grid%a_snowncv, &
                      grid%graupelnc, grid%a_graupelnc, grid%graupelncv, grid%a_graupelncv, &
                      grid%nca,grid%swrad_scat,                    &
                      grid%cldefi,grid%lowlyr,                          &
                      grid%mass_flux,                              &
                      grid%rthften, grid%rqvften,                       &
                      grid%cldfra,                                      &
#if (WRF_CHEM!=1)
                      cldfra_old,                                  &
#endif
                      grid%glw,grid%gsw,grid%emiss,grid%embck,            &
                      grid%lu_index,                                      &
                      grid%landuse_ISICE, grid%landuse_LUCATS,            &
                      grid%landuse_LUSEAS, grid%landuse_ISN,              &
                      grid%lu_state,                                      &
                      grid%xlat,grid%xlong,grid%albedo,grid%albbck,grid%GMT,grid%JULYR,grid%JULDAY,     &
                      grid%levsiz, num_ozmixm, num_aerosolc, grid%paerlev,  &
                      grid%alevsiz, grid%no_src_types,                      &
                      grid%tmn,grid%xland,grid%znt,grid%z0,grid%ust,grid%mol,grid%pblh,grid%tke_pbl,    &
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
                      config_flags%num_urban_hi,                            & !multi-layer urban
                      grid%raincv_a,grid%raincv_b,                                    &
                      grid%gd_cloud, grid%gd_cloud2,                                  & ! Optional
                      grid%gd_cloud_a, grid%gd_cloud2_a,                              & ! Optional
                      grid%QC_CU, grid%QI_CU,                                         & ! Optional
                      ozmixm,grid%pin,                             &     ! Optional
                      grid%m_ps_1,grid%m_ps_2,grid%m_hybi,aerosolc_1,aerosolc_2,&  ! Optional
                      grid%rundgdten,grid%rvndgdten,grid%rthndgdten,         &     ! Optional
                      grid%rphndgdten,grid%rqvndgdten,grid%rmundgdten,       &     ! Optional
                      grid%FGDT,grid%stepfg,                        &     ! Optional
                      grid%cugd_tten,grid%cugd_ttens,grid%cugd_qvten,        &   ! Optional
                      grid%cugd_qvtens,grid%cugd_qcten,                 &   ! Optional
                      grid%ISNOWXY, grid%ZSNSOXY, grid%TSNOXY,                                  &   ! Optional Noah-MP
                      grid%SNICEXY, grid%SNLIQXY, grid%TVXY, grid%TGXY, grid%CANICEXY,          &   ! Optional Noah-MP
                      grid%CANLIQXY, grid%EAHXY, grid%TAHXY, grid%CMXY,                         &   ! Optional Noah-MP
                      grid%CHXY, grid%FWETXY, grid%SNEQVOXY, grid%ALBOLDXY, grid%QSNOWXY,       &   ! Optional Noah-MP
                      grid%WSLAKEXY, grid%ZWTXY, grid%WAXY, grid%WTXY, grid%LFMASSXY, grid%RTMASSXY, & ! Optional Noah-MP
                      grid%STMASSXY, grid%WOODXY, grid%STBLCPXY, grid%FASTCPXY,                 &   ! Optional Noah-MP
                      grid%XSAIXY,                                                              &   ! Optional Noah-MP
                      grid%T2MVXY, grid%T2MBXY, grid%CHSTARXY,                                  &   ! Optional Noah-MP
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
                      ,grid%te_temf,grid%cf3d_temf,grid%wm_temf        & ! WA
                      ,grid%massflux_EDKF, grid%entr_EDKF, grid%detr_EDKF                & 
                      ,grid%thl_up,grid%thv_up,grid%rt_up                                &
                      ,grid%rv_up,grid%rc_up,grid%u_up,grid%v_up,grid%frac_up            &
                      )
       ENDIF   !tgs

   ENDDO

   CALL wrf_debug ( 100 , 'start_domain_em_ad: After call to a_phy_init' )

   END IF

!
!-------------

   IF(.not.config_flags%restart)THEN

    DO j=jts,min(jte,jde-1)
       DO i=its,min(ite,ide-1)
          DO k=kte, kts+1, -1
             grid%a_p(i,k-1,j) = grid%a_p(i,k-1,j) + 2.*grid%a_p_hyd_w(i,k,j)
             grid%a_p_hyd_w(i,k-1,j) = grid%a_p_hyd_w(i,k-1,j) - grid%a_p_hyd_w(i,k,j)
             grid%a_p_hyd_w(i,k,j) = 0.0
          ENDDO
          grid%a_p(i,1,j) = grid%a_p(i,1,j) + grid%a_p_hyd_w(i,1,j)
          grid%a_p_hyd_w(i,1,j) = 0.0
       ENDDO
    ENDDO

!  reconstitute base-state fields

     IF ( first_trip_for_this_domain ) THEN
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
             grid%mu_1(i,j)=grid%mu_2(i,j)
           ENDIF
       ENDDO
       ENDDO
     END IF 

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
    ENDIF

! Use equations from calc_p_rho_phi to derive p and al from ph

    IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
       DO j=jts,min(jte,jde-1)
       DO k=kts,kte-1
       DO i=its,min(ite,ide-1)
          grid%al(i,k,j)=-1./(grid%mub(i,j)+grid%mu_1(i,j))*(grid%alb(i,k,j)*grid%mu_1(i,j)  &
                     +grid%rdnw(k)*(grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j)))
       ENDDO
       ENDDO
       ENDDO
    ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
       DO j=jts,min(jte,jde-1)
       DO k=kts,kte-1
       DO i=its,min(ite,ide-1)
          pfu = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znw(k+1)+grid%p_top
          pfd = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znw(k)  +grid%p_top
          phm = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znu(k)  +grid%p_top
          grid%al(i,k,j) = (grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j)+grid%phb(i,k+1,j)-grid%phb(i,k,j)) &
                            /phm/LOG(pfd/pfu)-grid%alb(i,k,j)
       ENDDO
       ENDDO
       ENDDO
    END IF

    DO j=min(jte,jde-1),jts,-1
    DO k=kte-1,kts,-1
    DO i=min(ite,ide-1),its,-1
        grid%a_p(i,k,j) = grid%a_p(i,k,j) + grid%a_p_hyd(i,k,j)
        IF ( .NOT. config_flags%var4d_run ) THEN
           qvf = 1. + rvovrd*grid%moist(i,k,j,P_QV)
           temp2 = p1000mb*(grid%alb(i,k,j)+grid%al(i,k,j))
           temp1 = t0 + grid%t_1(i,k,j)
           temp0 = temp1*qvf/temp2
           IF (r_d*temp0 .LE. 0.0 .AND. (cpovcv .EQ. 0.0 .OR. cpovcv .NE. &
                INT(cpovcv))) THEN
             temp0b = 0.0
           ELSE
             temp0b = r_d*cpovcv*(r_d*temp0)**(cpovcv-1)*p1000mb*grid%a_p(i,k,j)/temp2
           END IF
           grid%a_t_1(i,k,j) = grid%a_t_1(i,k,j) + qvf*temp0b
           a_qvf = temp1*temp0b
           grid%a_p(i,k,j) = 0.0
           grid%a_moist(i,k,j,P_QV) = grid%a_moist(i,k,j,P_QV) + rvovrd*a_qvf
        END IF
        IF ( .NOT. config_flags%var4d_run ) THEN
           grid%a_al(i,k,j) = grid%a_al(i,k,j) + grid%a_alt(i,k,j) - temp0*p1000mb*temp0b
        ELSE
           grid%a_al(i,k,j) = grid%a_al(i,k,j) + grid%a_alt(i,k,j)
        END IF
        grid%a_alt(i,k,j) = 0.0
        grid%a_p_hyd(i,k,j) = 0.0
    ENDDO
    ENDDO
    ENDDO

    IF ( config_flags%hypsometric_opt .EQ. 1 ) THEN
       DO j=min(jte,jde-1),jts,-1
       DO k=kte-1,kts,-1
       DO i=min(ite,ide-1),its,-1
          temp = grid%mub(i,j) + grid%mu_1(i,j)
          tempb = -(grid%a_al(i,k,j)/temp)
          grid%a_mu_1(i,j) = grid%a_mu_1(i,j) + (grid%alb(i,k,j)-(grid%alb(i,k,j)*grid%mu_1(i,j) &
             +grid%rdnw(k)*(grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j)))/temp)*tempb
          grid%a_ph_1(i,k+1,j) = grid%a_ph_1(i,k+1,j) + grid%rdnw(k)*tempb
          grid%a_ph_1(i,k,j) = grid%a_ph_1(i,k,j) - grid%rdnw(k)*tempb
          grid%a_al(i,k,j) = 0.0
       ENDDO
       ENDDO
       ENDDO
    ELSE IF ( config_flags%hypsometric_opt .EQ. 2 ) THEN
       DO j=min(jte,jde-1),jts,-1
       DO k=kte-1,kts,-1
       DO i=min(ite,ide-1),its,-1
          pfu = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znw(k+1) + grid%p_top
          phm = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znu(k) + grid%p_top
          pfd = (grid%mub(i,j)+grid%mu_1(i,j))*grid%znw(k) + grid%p_top
          temp0 = pfd/pfu
          temp1 = LOG(temp0)
          temp = phm*temp1
          tempb = grid%a_al(i,k,j)/temp
          tempb0 = -((grid%phb(i,k+1,j)-grid%phb(i,k,j)+grid%ph_1(i,k+1,j)-grid%ph_1(i,k,j))*tempb/temp)
          temp0b = phm*tempb0/(temp0*pfu)
          grid%a_ph_1(i,k+1,j) = grid%a_ph_1(i,k+1,j) + tempb
          grid%a_ph_1(i,k,j) = grid%a_ph_1(i,k,j) - tempb
          a_phm = temp1*tempb0
          a_pfd = temp0b
          a_pfu = -(temp0*temp0b)
          grid%a_al(i,k,j) = 0.0
          grid%a_mu_1(i,j) = grid%a_mu_1(i,j) + grid%znw(k)*a_pfd + grid%znw(k+1)*a_pfu + grid%znu(k)*a_phm
       ENDDO
       ENDDO
       ENDDO
    END IF

!  if this is for a nested domain, the defined/interpolated fields are the _2

     IF ( first_trip_for_this_domain ) THEN

! data that is expected to be zero must be explicitly initialized as such
!    grid%h_diabatic = 0.

       DO j = jts,min(jte,jde-1)
       DO k = kts,kte-1
       DO i = its, min(ite,ide-1)
           IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
             grid%a_t_2(i,k,j) = grid%a_t_2(i,k,j) + grid%a_t_1(i,k,j)
             grid%a_t_1(i,k,j) = 0.0
           ENDIF
       ENDDO
       ENDDO
       ENDDO

       DO j = jts,min(jte,jde-1)
       DO k = kts,kte
       DO i = its, min(ite,ide-1)
          grid%a_ph_2(i,k,j) = grid%a_ph_2(i,k,j) + grid%a_ph_1(i,k,j)
          grid%a_ph_1(i,k,j) = 0.0
       ENDDO
       ENDDO
       ENDDO

       DO j = jts,min(jte,jde-1)
       DO i = its, min(ite,ide-1)
           IF ( grid%imask_nostag(i,j) .EQ. 1 ) THEN
             grid%a_mu_2(i,j) = grid%a_mu_2(i,j) + grid%a_mu_1(i,j)
             grid%a_mu_1(i,j) = 0.0
           ENDIF
       ENDDO
       ENDDO
     END IF

   ENDIF


#if (WRF_CHEM!=1)
      DEALLOCATE(CLDFRA_OLD)
#endif

     CALL wrf_debug ( 100 , 'start_domain_em_ad: Returning' )

     RETURN

   END SUBROUTINE a_start_domain_em

