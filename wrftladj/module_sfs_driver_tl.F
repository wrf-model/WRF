!WRF+/TL:MEDIATION_LAYER:PHYSICS
!Created by Ning Pan, 2010-08 

MODULE g_module_sfs_driver

CONTAINS

SUBROUTINE g_sfs_driver( grid, config_flags, &
                            nba_mij,g_nba_mij, n_nba_mij, & 
                            nba_rij,g_nba_rij, n_nba_rij  )

  USE module_domain
  USE module_configure
  USE module_tiles
  USE module_machine
  USE module_state_description
  USE g_module_bc
  USE module_sfs_nba, ONLY : calc_mij_constants
  USE g_module_sfs_nba
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
  :: nba_mij,g_nba_mij

  REAL ,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,n_nba_rij) &
  :: nba_rij,g_nba_rij

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

  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )


  IF ( (config_flags%sfs_opt .EQ. 1) .OR. (config_flags%sfs_opt .EQ. 2) ) THEN

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !---------------------------------------- 

        CALL calc_mij_constants( )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !---------------------------------------- 

        CALL g_calc_smnsmn( nba_rij(ims,kms,jms,P_smnsmn),g_nba_rij(ims,kms,jms,P_smnsmn),    &
                          grid%defor11,grid%g_defor11, grid%defor22,grid%g_defor22,  &
                          grid%defor33,grid%g_defor33, grid%defor12,grid%g_defor12,  &
                          grid%defor13,grid%g_defor13, grid%defor23,grid%g_defor23,  &
                          config_flags,                     &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          ips, ipe, jps, jpe, kps, kpe,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start    , k_end                )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO


#ifdef DM_PARALLEL
#      include "HALO_EM_NBA_RIJ.inc"
#      include "PERIOD_EM_NBA_RIJ.inc"
#endif

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

        CALL g_set_physical_bc3d( nba_rij(ims,kms,jms,P_r12),g_nba_rij(ims,kms,jms,P_r12), 'd',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

        CALL g_set_physical_bc3d( nba_rij(ims,kms,jms,P_r13),g_nba_rij(ims,kms,jms,P_r13), 'e',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

        CALL g_set_physical_bc3d( nba_rij(ims,kms,jms,P_r23),g_nba_rij(ims,kms,jms,P_r23), 'f',  &
                                config_flags,                     &
                                ids, ide, jds, jde, kds, kde,     &
                                ims, ime, jms, jme, kms, kme,     &
                                ips, ipe, jps, jpe, kps, kpe,     &
                                grid%i_start(ij), grid%i_end(ij), &
                                grid%j_start(ij), grid%j_end(ij), &
                                k_start    , k_end                )

        CALL g_set_physical_bc3d( nba_rij(ims,kms,jms,P_smnsmn),g_nba_rij(ims,kms,jms,P_smnsmn), 'c', &
                                config_flags,                       &
                                ids, ide, jds, jde, kds, kde,       &
                                ims, ime, jms, jme, kms, kme,       &
                                ips, ipe, jps, jpe, kps, kpe,       &
                                grid%i_start(ij), grid%i_end(ij),   &
                                grid%j_start(ij), grid%j_end(ij),   &
                                k_start    , k_end                  )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL g_calc_mii( nba_mij(ims,kms,jms,P_m11),g_nba_mij(ims,kms,jms,P_m11), &
                          nba_mij(ims,kms,jms,P_m22),g_nba_mij(ims,kms,jms,P_m22), &
                          nba_mij(ims,kms,jms,P_m33),g_nba_mij(ims,kms,jms,P_m33), &
                          grid%defor11,grid%g_defor11, grid%defor22,grid%g_defor22,   &
                          grid%defor33,grid%g_defor33, grid%defor12,grid%g_defor12,   &
                          grid%defor13,grid%g_defor13, grid%defor23,grid%g_defor23,   &
                          nba_rij(ims,kms,jms,P_r12),g_nba_rij(ims,kms,jms,P_r12), &
                          nba_rij(ims,kms,jms,P_r13),g_nba_rij(ims,kms,jms,P_r13), &
                          nba_rij(ims,kms,jms,P_r23),g_nba_rij(ims,kms,jms,P_r23), &
                          nba_rij(ims,kms,jms,P_smnsmn),g_nba_rij(ims,kms,jms,P_smnsmn),     &
                          grid%tke_2,grid%g_tke_2,                      & 
                          grid%rdzw,grid%g_rdzw, grid%dx, grid%dy,      &
                          config_flags,                     &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          ips, ipe, jps, jpe, kps, kpe,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                    )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL g_calc_m12( nba_mij(ims,kms,jms,P_m12),g_nba_mij(ims,kms,jms,P_m12), &
                          grid%defor11,grid%g_defor11, grid%defor22,grid%g_defor22,   &
                          grid%defor12,grid%g_defor12,       &
                          grid%defor13,grid%g_defor13, grid%defor23,grid%g_defor23,   &
                          nba_rij(ims,kms,jms,P_r12),g_nba_rij(ims,kms,jms,P_r12), &
                          nba_rij(ims,kms,jms,P_r13),g_nba_rij(ims,kms,jms,P_r13), &
                          nba_rij(ims,kms,jms,P_r23),g_nba_rij(ims,kms,jms,P_r23), &
                          nba_rij(ims,kms,jms,P_smnsmn),g_nba_rij(ims,kms,jms,P_smnsmn),     &
                          grid%tke_2,grid%g_tke_2,                      & 
                          grid%rdzw,grid%g_rdzw, grid%dx, grid%dy,      &
                          config_flags,                     &
                          ids, ide, jds, jde, kds, kde,     &
                          ims, ime, jms, jme, kms, kme,     &
                          ips, ipe, jps, jpe, kps, kpe,     &
                          grid%i_start(ij), grid%i_end(ij), &
                          grid%j_start(ij), grid%j_end(ij), &
                          k_start, k_end                    )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL g_calc_m13( nba_mij(ims,kms,jms,P_m13),g_nba_mij(ims,kms,jms,P_m13), &
                          grid%defor11,grid%g_defor11,       &
                          grid%defor33,grid%g_defor33, grid%defor12,grid%g_defor12,   &
                          grid%defor13,grid%g_defor13, grid%defor23,grid%g_defor23,   &
                          nba_rij(ims,kms,jms,P_r12),g_nba_rij(ims,kms,jms,P_r12), &
                          nba_rij(ims,kms,jms,P_r13),g_nba_rij(ims,kms,jms,P_r13), &
                          nba_rij(ims,kms,jms,P_r23),g_nba_rij(ims,kms,jms,P_r23), &
                          nba_rij(ims,kms,jms,P_smnsmn),g_nba_rij(ims,kms,jms,P_smnsmn),     &
                          grid%tke_2,grid%g_tke_2,                      & 
                          grid%rdzw,grid%g_rdzw, grid%dx, grid%dy,      &
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

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL g_calc_m23( nba_mij(ims,kms,jms,P_m23),g_nba_mij(ims,kms,jms,P_m23),     &
                          grid%defor22,grid%g_defor22,       &
                          grid%defor33,grid%g_defor33, grid%defor12,grid%g_defor12,       &
                          grid%defor13,grid%g_defor13, grid%defor23,grid%g_defor23,       &
                          nba_rij(ims,kms,jms,P_r12),g_nba_rij(ims,kms,jms,P_r12),     &
                          nba_rij(ims,kms,jms,P_r13),g_nba_rij(ims,kms,jms,P_r13),     &
                          nba_rij(ims,kms,jms,P_r23),g_nba_rij(ims,kms,jms,P_r23),     &
                          nba_rij(ims,kms,jms,P_smnsmn),g_nba_rij(ims,kms,jms,P_smnsmn),     &
                          grid%tke_2,grid%g_tke_2,                      & 
                          grid%rdzw,grid%g_rdzw, grid%dx, grid%dy,      &
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

#ifdef DM_PARALLEL
#      include "HALO_EM_NBA_MIJ.inc"
#      include "PERIOD_EM_NBA_MIJ.inc"
#endif

    !$OMP PARALLEL DO   &
    !$OMP PRIVATE ( ij )
    DO ij = 1 , grid%num_tiles !----------------------------------------

      CALL g_set_physical_bc3d( nba_mij(ims,kms,jms,P_m11),g_nba_mij(ims,kms,jms,P_m11), 'p',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )
      
      CALL g_set_physical_bc3d( nba_mij(ims,kms,jms,P_m22),g_nba_mij(ims,kms,jms,P_m22), 'p',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

      CALL g_set_physical_bc3d( nba_mij(ims,kms,jms,P_m33),g_nba_mij(ims,kms,jms,P_m33), 'p',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

      CALL g_set_physical_bc3d( nba_mij(ims,kms,jms,P_m12),g_nba_mij(ims,kms,jms,P_m12), 'd',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

      CALL g_set_physical_bc3d( nba_mij(ims,kms,jms,P_m13),g_nba_mij(ims,kms,jms,P_m13), 'e',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

      CALL g_set_physical_bc3d( nba_mij(ims,kms,jms,P_m23),g_nba_mij(ims,kms,jms,P_m23), 'f',    &
                              config_flags,                     &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              ips, ipe, jps, jpe, kps, kpe,     &
                              grid%i_start(ij), grid%i_end(ij), &
                              grid%j_start(ij), grid%j_end(ij), &
                              k_start    , k_end                )

    ENDDO !-------------------------------------------------------------
    !$OMP END PARALLEL DO

  ENDIF !(config_flags%sfs_opt .EQ. 1) .OR. (config_flags%sfs_opt .EQ. 2)

END SUBROUTINE g_sfs_driver

END MODULE g_module_sfs_driver
