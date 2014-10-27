!WRF:MEDIATION_LAYER:SOLVER

MODULE module_after_all_rk_steps

CONTAINS

   !  This subroutine is called once per domain per time step.  It is outside
   !  of and after the end of the Runge-Kutta time steps, after the calls to 
   !  the explicit moisture driver, and after the polar filtering calls.  The
   !  variables in here are all up-to-date with the end of this current time 
   !  step.


   SUBROUTINE after_all_rk_steps ( grid, config_flags,                  &
                                   moist, chem, tracer, scalar,         &
                                   th_phy, pi_phy, p_phy, rho_phy,      & 
                                   p8w, t8w, dz8w,                      &
                                   curr_secs2,                          &
                                   diag_flag,                           &
                                   ids,  ide,  jds,  jde,  kds,  kde,   &
                                   ims,  ime,  jms,  jme,  kms,  kme,   &
                                   ips,  ipe,  jps,  jpe,  kps,  kpe,   &
                                   imsx, imex, jmsx, jmex, kmsx, kmex,  &
                                   ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                                   imsy, imey, jmsy, jmey, kmsy, kmey,  &
                                   ipsy, ipey, jpsy, jpey, kpsy, kpey   )


      !=============================================================
      !  USE Association for Generic WRF Infrastructure
      !=============================================================

      !  Pick up the number of members for each of the 4d arrays - for declaration purposes.

      USE module_state_description, ONLY: num_moist, num_chem, num_tracer, num_scalar

      !  This gives us the type definition for grid (domain)

      USE module_domain, ONLY : domain

      !  All of the information from the namelist is in config_flags.  The
      !  type declaration for this puppy must be available.

      USE module_configure, ONLY : grid_config_rec_type

#ifdef DM_PARALLEL
      !  Ensure some of the fancy diagnostics variables that need to
      !  talk to other patches can do so.

      USE module_dm, ONLY : &
                  local_communicator, mytask, ntasks, ntasks_x, ntasks_y                   &
                 ,local_communicator_periodic, wrf_dm_maxval

      USE module_comm_dm, ONLY : &
                  halo_em_phys_w_sub
#endif

      !=============================================================
      !  USE Association for the Diagnostic Packages
      !=============================================================
      
      USE module_diagnostics_driver, ONLY : diagnostics_driver


      IMPLICIT NONE


      !=============================================================
      !  Subroutine Arguments
      !=============================================================

      !  Arguments passed in.  All of the diagnostics are part of the grid structure, so
      !  even though we are not changing any of the fundamental variables, we are computing
      !  the diagnostics.  Therefore grid is INOUT.

      TYPE ( domain ), INTENT(INOUT) :: grid

      !  We are not changing any of the namelist settings.

      TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags

      !  The 4d arrays are input only, no mods to them.

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_moist ) , INTENT(IN) :: moist
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_chem  ) , INTENT(IN) :: chem
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_tracer) , INTENT(IN) :: tracer
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme,num_scalar) , INTENT(IN) :: scalar

      !  A few handy 3d arrays computed for the physics scheme: pressure (Pa) and
      !  temperature (K), on both half (_phy) and full levels.

      REAL , DIMENSION(ims:ime,kms:kme,jms:jme)            , INTENT(IN) :: th_phy  , &
                                                                           p_phy   , &
                                                                           pi_phy  , &
                                                                           rho_phy , &
                                                                           dz8w    , &
                                                                           p8w     , &
                                                                           t8w

      !  Time (s) since the beginning of the restart.

      REAL :: curr_secs2

      !  Is this to be a history output time?  If so, compute the diagnostics.

      LOGICAL :: diag_flag

      !  The sundry dimensions required to keep a model running smoothly:
      !     The first letter:
      !        i: refers to the nominally west east direction, the inner-most (fastest)
      !           incrementing index
      !        j: refers to the nominally south north direction, the outer-most (slowest)
      !           incrementing index
      !        k: refers to the vertical direction form bottom to top, the second dimension
      !           in all 3d arrays
      !     The second letter: 
      !        d: refers to the domain size, the geophysical extent of the entire domain,
      !           not used in dimensions or looping, used to determine when we are close to
      !           the edge of the boundary
      !        m: refers to the memory size size, all 2d and 3d arrays from the Registry
      !           (passed into here via the grid structure or the I1 variables [such as
      !           p_phy, for example]) use these values for dimensioning
      !        p: refers to the patch size, the extent over which computational loops run

      INTEGER , INTENT(IN) :: ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe

      !  Hopefully unnecessary, these are the filtered dimensions.

      INTEGER , INTENT(IN) :: imsx,imex,jmsx,jmex,kmsx,kmex,    &
                              ipsx,ipex,jpsx,jpex,kpsx,kpex,    &
                              imsy,imey,jmsy,jmey,kmsy,kmey,    &
                              ipsy,ipey,jpsy,jpey,kpsy,kpey

#ifdef DM_PARALLEL
      !=============================================================
      !  Include patch communications
      !=============================================================
#     include "HALO_EM_PHYS_W.inc"
#endif

      !=============================================================
      !  Start of executable code
      !=============================================================

      CALL wrf_debug ( 100 , '--> TOP OF AFTER ALL RK STEPS' ) 
      CALL wrf_debug ( 100 , '--> CALLING DIAGNOSTICS DRIVER' )

      CALL diagnostics_driver ( grid, config_flags,               &
                                moist, chem, tracer, scalar,         &
                                th_phy, pi_phy, p_phy, rho_phy,      & 
                                p8w, t8w, dz8w,                      &
                                curr_secs2,                          &
                                diag_flag,                           &
                                ids,  ide,  jds,  jde,  kds,  kde,   &
                                ims,  ime,  jms,  jme,  kms,  kme,   &
                                ips,  ipe,  jps,  jpe,  kps,  kpe,   &
                                imsx, imex, jmsx, jmex, kmsx, kmex,  &
                                ipsx, ipex, jpsx, jpex, kpsx, kpex,  &
                                imsy, imey, jmsy, jmey, kmsy, kmey,  &
                                ipsy, ipey, jpsy, jpey, kpsy, kpey   )


   END SUBROUTINE after_all_rk_steps

END MODULE module_after_all_rk_steps
