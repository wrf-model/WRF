!WRF:MEDIATION_LAYER:PHYSICS

!==============================================================================
!
! Copyright 2009. Lawrence Livermore National Security, LLC. All rights reserved.
! This work was produced at the Lawrence Livermore National Laboratory (LLNL) under
! contract no. DE-AC52-07NA27344 (Contract 44) between the U.S. Department of Energy (DOE)
! and Lawrence Livermore National Security, LLC (LLNS) for the operation of LLNL. Copyright
! is reserved to Lawrence Livermore National Security, LLC for purposes of controlled
! dissemination, commercialization through formal licensing, or other disposition under
! terms of Contract 44; DOE policies, regulations and orders; and U.S. statutes. The rights
! of the Federal Government are reserved under Contract 44.
!
! DISCLAIMER
! This work was prepared as an account of work sponsored by an agency of the United States
! Government. Neither the United States Government nor Lawrence Livermore National
! Security, LLC nor any of their employees, makes any warranty, express or implied, or
! assumes any liability or responsibility for the accuracy, completeness, or usefulness of
! any information, apparatus, product, or process disclosed, or represents that its use
! would not infringe privately-owned rights. Reference herein to any specific commercial
! products, process, or service by trade name, trademark, manufacturer or otherwise does
! not necessarily constitute or imply its endorsement, recommendation, or favoring by the
! United States Government or Lawrence Livermore National Security, LLC. The views and
! opinions of authors expressed herein do not necessarily state or reflect those of the
! United States Government or Lawrence Livermore National Security, LLC, and shall not be
! used for advertising or product endorsement purposes.
!
! LICENSING REQUIREMENTS
! Any use, reproduction, modification, or distribution of this software or documentation
! for commercial purposes requires a license from Lawrence Livermore National Security,
! LLC. Contact: Lawrence Livermore National Laboratory, Industrial Partnerships Office,
! P.O. Box 808, L-795, Livermore, CA 94551
!
!=============================================================================
!
! Modification History: 
!
! Implemented 12/2009 by Jeff Mirocha, jmirocha@llnl.gov
!
!=============================================================================

MODULE module_sfs_driver

CONTAINS

!=============================================================================

SUBROUTINE sfs_driver( grid, config_flags, &
                       nba_mij, n_nba_mij, & 
                       nba_rij, n_nba_rij  )

!-----------------------------------------------------------------------------
!
! PURPOSE: Calls turbulence subfilter stress model subroutines and handles
!          all MPI and OMP operations
!
!-----------------------------------------------------------------------------


! Driver layer modules
  USE module_domain
  USE module_model_constants
  USE module_configure
  USE module_tiles
  USE module_machine
  USE module_state_description
! Model layer modules
  USE module_bc

!! *** add new modules of schemes here

  USE module_sfs_nba
#ifdef DM_PARALLEL
   USE module_dm
   USE module_comm_dm, ONLY : &
                           HALO_EM_NBA_RIJ_sub   &
                          ,PERIOD_EM_NBA_RIJ_sub   &
                          ,HALO_EM_NBA_MIJ_sub   &
                          ,PERIOD_EM_NBA_MIJ_sub
#endif

  IMPLICIT NONE

! Input data.

  TYPE(domain) , TARGET          :: grid

  TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags

  INTEGER, INTENT(  IN ) :: n_nba_mij, n_nba_rij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_mij) &
  :: nba_mij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_rij) &
  :: nba_rij

! Local data

  INTEGER :: k_start , k_end, its, ite, jts, jte
  INTEGER :: ids , ide , jds , jde , kds , kde , &
             ims , ime , jms , jme , kms , kme , &
             ips , ipe , jps , jpe , kps , kpe

  INTEGER :: imsx, imex, jmsx, jmex, kmsx, kmex, &
             ipsx, ipex, jpsx, jpex, kpsx, kpex, &
             imsy, imey, jmsy, jmey, kmsy, kmey, &
             ipsy, ipey, jpsy, jpey, kpsy, kpey
 
  INTEGER :: ij, i, j, k

  CALL get_ijk_from_grid ( grid ,                              &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme,       &
                           ips, ipe, jps, jpe, kps, kpe,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, kpex, &
                           imsy, imey, jmsy, jmey, kmsy, kmey, &
                           ipsy, ipey, jpsy, jpey, kpsy, kpey  )

  k_start         = kps
  k_end           = kpe

! Compute these starting and stopping locations for each tile and number of tiles.
! See: http://www.mmm.ucar.edu/wrf/WG2/topics/settiles

! Solve_em has already called this, so should not be necessary to reset tiles here
  CALL set_tiles ( ZONE_SFS, grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

  IF ( (config_flags%sfs_opt .EQ. 1) .OR. (config_flags%sfs_opt .EQ. 2) ) THEN

!=======================================================================
!
!                                BEGIN NBA
!
!=======================================================================
      
!  IF ( grid%itimestep .EQ. 1 ) THEN
!         
!    IF ( (config_flags%sfs_opt .EQ. 2) .AND. (config_flags%km_opt .NE. 2)) THEN
!  
!    CALL wrf_error_fatal( 'Must use km_opt=2 with sfs_opt=2' )
!    
!    ENDIF
!
!  ENDIF

!_______________________________________________________________________
!
! Compute NBA model constants
!_______________________________________________________________________


    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !---------------------------------------- 

        CALL calc_mij_constants( )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

!_______________________________________________________________________
!
! Compute Smn*Smn
!_______________________________________________________________________

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !---------------------------------------- 

        CALL calc_smnsmn( nba_rij(ims,kms,jms,P_smnsmn),    &
                          grid%defor11, grid%defor22,       &
                          grid%defor33, grid%defor12,       &
                          grid%defor13, grid%defor23,       &
                          config_flags,                     &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          ips, ipe, jps, jpe, kps, kpe,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end                )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

!_______________________________________________________________________
!
! Update halos for R12, R13, R23 and smnsmn
!_______________________________________________________________________

#ifdef DM_PARALLEL
#      include "HALO_EM_NBA_RIJ.inc"
#      include "PERIOD_EM_NBA_RIJ.inc"
#endif

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

        CALL set_physical_bc3d( nba_rij(ims,kms,jms,P_r12), 'd',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )


        CALL set_physical_bc3d( nba_rij(ims,kms,jms,P_r13), 'e',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

        CALL set_physical_bc3d( nba_rij(ims,kms,jms,P_r23), 'f',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

        CALL set_physical_bc3d( nba_rij(ims,kms,jms,P_smnsmn), 'c', &
                                config_flags,                       &
                                ids, ide, jds, jde, kds, kde,       &
                                ims, ime, jms, jme, kms, kme,       &
                                ips, ipe, jps, jpe, kps, kpe,       &
                                grid%i_start(ij), grid%i_end(ij),   &
                                grid%j_start(ij), grid%j_end(ij),   &
                                k_start    , k_end                  )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

!_______________________________________________________________________
!
! Calculate M11, M22 and M33
!_______________________________________________________________________

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL calc_mii( nba_mij(ims,kms,jms,P_m11),       &
                     nba_mij(ims,kms,jms,P_m22),       &
                     nba_mij(ims,kms,jms,P_m33),       &
                     grid%defor11, grid%defor22,       &
                     grid%defor33, grid%defor12,       &
                     grid%defor13, grid%defor23,       &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

!_______________________________________________________________________
!
! Calculate M12
!_______________________________________________________________________

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL calc_m12( nba_mij(ims,kms,jms,P_m12),       &
                     grid%defor11, grid%defor22,       &
                     grid%defor12, grid%defor13,       &
                     grid%defor23,                     &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

!_______________________________________________________________________
!
! Calculate M13
!_______________________________________________________________________

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL calc_m13( nba_mij(ims,kms,jms,P_m13),       &
                     grid%defor11, grid%defor33,       &
                     grid%defor12, grid%defor13,       &
                     grid%defor23,                     &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     grid%fnm, grid%fnp,               &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO
!_______________________________________________________________________
!
! Calculate M23
!_______________________________________________________________________

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL calc_m23( nba_mij(ims,kms,jms,P_m23),       &
                     grid%defor22, grid%defor33,       &
                     grid%defor12, grid%defor13,       &
                     grid%defor23,                     &
                     nba_rij(ims,kms,jms,P_r12),       &
                     nba_rij(ims,kms,jms,P_r13),       &
                     nba_rij(ims,kms,jms,P_r23),       &
                     nba_rij(ims,kms,jms,P_smnsmn),    &
                     grid%tke_2,                       & 
                     grid%rdzw, grid%dx, grid%dy,      &
                     grid%fnm, grid%fnp,               &
                     config_flags,                     &
                     ids, ide, jds, jde, kds, kde,     &
                     ims, ime, jms, jme, kms, kme,     &
                     ips, ipe, jps, jpe, kps, kpe,     &
                     grid%i_start(ij), grid%i_end(ij), &
                     grid%j_start(ij), grid%j_end(ij), &
                     k_start, k_end                    )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO
!_______________________________________________________________________
!
! Update boundary conditions and halos after calculating Mij
!_______________________________________________________________________

#ifdef DM_PARALLEL
#      include "HALO_EM_NBA_MIJ.inc"
#      include "PERIOD_EM_NBA_MIJ.inc"
#endif

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL set_physical_bc3d( nba_mij(ims,kms,jms,P_m11), 'p',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )
      
      CALL set_physical_bc3d( nba_mij(ims,kms,jms,P_m22), 'p',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )
      
      CALL set_physical_bc3d( nba_mij(ims,kms,jms,P_m33), 'p',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

      CALL set_physical_bc3d( nba_mij(ims,kms,jms,P_m12), 'd',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

      CALL set_physical_bc3d( nba_mij(ims,kms,jms,P_m13), 'e',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

      CALL set_physical_bc3d( nba_mij(ims,kms,jms,P_m23), 'f',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

!=======================================================================
!
!                                END NBA
!
!=======================================================================

  ENDIF !(config_flags%sfs_opt .EQ. 1) .OR. (config_flags%sfs_opt .EQ. 2)

END SUBROUTINE sfs_driver

END MODULE module_sfs_driver
