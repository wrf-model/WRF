
!------------------------------------------------------------------

#if ( EM_CORE == 1 && DA_CORE != 1 )

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_small_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_comm_dm, ONLY: halo_em_horiz_interp_sub
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, &
                            mytask, get_dm_max_halo_width,                          &
                            nest_task_offsets, mpi_comm_to_kid, mpi_comm_to_mom,    &
                            which_kid, nest_pes_x, nest_pes_y, intercomm_active
      USE module_timing
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      INTEGER iparstrt,jparstrt,sw
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::           ids,  ide,  jds,  jde,  kds,  kde,    &
                                 ims,  ime,  jms,  jme,  kms,  kme,    &
                                 ips,  ipe,  jps,  jpe,  kps,  kpe

      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER icoord, jcoord, idim_cd, jdim_cd, pgr
      INTEGER thisdomain_max_halo_width
      INTEGER local_comm, myproc, nproc
      INTEGER ioffset

      CALL wrf_get_dm_communicator ( local_comm )
      CALL wrf_get_myproc( myproc )
      CALL wrf_get_nproc( nproc )

      CALL get_ijk_from_grid (  grid ,                           &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe     )
#ifdef DM_PARALLEL
# include "HALO_EM_HORIZ_INTERP.inc"
#endif

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  intermediate_grid ,              &
                                iids, iide, ijds, ijde, ikds, ikde,    &
                                iims, iime, ijms, ijme, ikms, ikme,    &
                                iips, iipe, ijps, ijpe, ikps, ikpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      CALL nl_get_parent_grid_ratio ( ngrid%id, pgr )
      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = iide - iids + 1
      jdim_cd = ijde - ijds + 1

      nlev  = ckde - ckds + 1

      ! get max_halo_width for parent. It may be smaller if it is moad
      CALL get_dm_max_halo_width ( grid%id , thisdomain_max_halo_width )

      !  How many 3d arrays, so far just 3d theta-300 and geopotential perturbation,
      !  and the 2d topo elevation, three max press/temp/height fields, and three
      !  min press/temp/height fields.
   
      msize = ( 2 )* nlev + 7
   
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, calling rsl_lite_to_child')
      CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE     &
                              ,cips,cipe,cjps,cjpe                         &
                              ,iids,iide,ijds,ijde                         &
                              ,nids,nide,njds,njde                         &
                              ,pgr , sw                                    &
                              ,ntasks_x,ntasks_y                           &
                              ,thisdomain_max_halo_width                   &
                              ,icoord,jcoord                               &
                              ,idim_cd,jdim_cd                             &
                              ,pig,pjg,retval )
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, back from rsl_lite_to_child')
      DO while ( retval .eq. 1 )
         IF ( SIZE(grid%ph_2) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ph_2')
            DO k = ckds,ckde
               xv(k)= grid%ph_2(pig,k,pjg)
            END DO
            CALL rsl_lite_to_child_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%t_2) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, t_2')
            DO k = ckds,(ckde-1)
               xv(k)= grid%t_2(pig,k,pjg)
            END DO
            CALL rsl_lite_to_child_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%ht) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ht')
            xv(1)= grid%ht(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%t_max_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, t_max_p')
            xv(1)= grid%t_max_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%ght_max_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ght_max_p')
            xv(1)= grid%ght_max_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%max_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, max_p')
            xv(1)= grid%max_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%t_min_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, t_min_p')
            xv(1)= grid%t_min_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%ght_min_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, ght_min_p')
            xv(1)= grid%ght_min_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
         IF ( SIZE(grid%min_p) .GT. 1 ) THEN
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, min_p')
            xv(1)= grid%min_p(pig,pjg)
            CALL rsl_lite_to_child_msg(RWORDSIZE,xv)
         END IF
   
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, calling rsl_lite_to_child_info')
         CALL rsl_lite_to_child_info( local_communicator, msize*RWORDSIZE  &
                                     ,cips,cipe,cjps,cjpe                  &
                                     ,iids,iide,ijds,ijde                  &
                                     ,nids,nide,njds,njde                  &
                                     ,pgr , sw                             &
                                     ,ntasks_x,ntasks_y                    &
                                     ,thisdomain_max_halo_width            &
                                     ,icoord,jcoord                        &
                                     ,idim_cd,jdim_cd                      &
                                     ,pig,pjg,retval )
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, back from rsl_lite_to_child_info')
      END DO

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, calling rsl_lite_bcast')
      CALL rsl_lite_bcast_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                        nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                        ioffset, local_comm )
!call wrf_debug(0,'/external/RSL_LITE/module_dm.F, back from rsl_lite_bcast')

      RETURN
   END SUBROUTINE interp_domain_em_small_part1

!------------------------------------------------------------------

   SUBROUTINE interp_domain_em_small_part2 ( grid, ngrid, config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, &
                            mytask, get_dm_max_halo_width
      USE module_comm_nesting_dm, ONLY : halo_interp_down_sub
      IMPLICIT NONE
!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: ngrid
#include "dummy_new_decl.inc"
      INTEGER nlev, msize
      INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
      TYPE (grid_config_rec_type)            :: config_flags
      REAL xv(2000)
      INTEGER       ::          cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe
      INTEGER       ::          nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER myproc
      INTEGER ierr
      INTEGER thisdomain_max_halo_width

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )

      CALL rsl_lite_from_parent_info(pig,pjg,retval)
      
      DO while ( retval .eq. 1 )
      
         IF ( SIZE(grid%ph_2) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(((ckde)-(ckds)+1)*RWORDSIZE,xv)
            DO k = ckds,ckde
               grid%ph_2(pig,k,pjg) = xv(k)
            END DO
         END IF
   
         IF ( SIZE(grid%t_2) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg((((ckde-1))-(ckds)+1)*RWORDSIZE,xv)
            DO k = ckds,(ckde-1)
               grid%t_2(pig,k,pjg) = xv(k)
            END DO
         END IF
   
         IF ( SIZE(grid%ht) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%ht(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%t_max_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%t_max_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%ght_max_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%ght_max_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%max_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%max_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%t_min_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%t_min_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%ght_min_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%ght_min_p(pig,pjg) = xv(1)
         END IF
   
         IF ( SIZE(grid%min_p) .GT. 1 ) THEN
            CALL rsl_lite_from_parent_msg(RWORDSIZE,xv)
            grid%min_p(pig,pjg) = xv(1)
         END IF
      
         CALL rsl_lite_from_parent_info(pig,pjg,retval)
         
      END DO

      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_INTERP_DOWN.inc"

      CALL interp_fcn_bl ( grid%ph_2,                                           &       
                           cids, cide, ckds, ckde, cjds, cjde,                  &         
                           cims, cime, ckms, ckme, cjms, cjme,                  &         
                           cips, cipe, ckps, MIN( ckde, ckpe ), cjps, cjpe,     &         
                           ngrid%ph_2,                                          &   
                           nids, nide, nkds, nkde, njds, njde,                  &         
                           nims, nime, nkms, nkme, njms, njme,                  &         
                           nips, nipe, nkps, MIN( nkde, nkpe ), njps, njpe,     &         
                           config_flags%shw, ngrid%imask_nostag,                &         
                           .FALSE., .FALSE.,                                    &         
                           ngrid%i_parent_start, ngrid%j_parent_start,          &
                           ngrid%parent_grid_ratio, ngrid%parent_grid_ratio,    &
                           grid%ht, ngrid%ht,                                   &
                           grid%t_max_p, ngrid%t_max_p,                         &
                           grid%ght_max_p, ngrid%ght_max_p,                     &
                           grid%max_p, ngrid%max_p,                             &
                           grid%t_min_p, ngrid%t_min_p,                         &
                           grid%ght_min_p, ngrid%ght_min_p,                     &
                           grid%min_p, ngrid%min_p,                             &
                           ngrid%znw, ngrid%p_top                               )
      
      CALL interp_fcn_bl ( grid%t_2,                                            &       
                           cids, cide, ckds, ckde, cjds, cjde,                  &         
                           cims, cime, ckms, ckme, cjms, cjme,                  &         
                           cips, cipe, ckps, MIN( (ckde-1), ckpe ), cjps, cjpe, &         
                           ngrid%t_2,                                           &   
                           nids, nide, nkds, nkde, njds, njde,                  &         
                           nims, nime, nkms, nkme, njms, njme,                  &         
                           nips, nipe, nkps, MIN( (nkde-1), nkpe ), njps, njpe, &         
                           config_flags%shw, ngrid%imask_nostag,                &         
                           .FALSE., .FALSE.,                                    &         
                           ngrid%i_parent_start, ngrid%j_parent_start,          &
                           ngrid%parent_grid_ratio, ngrid%parent_grid_ratio,    &
                           grid%ht, ngrid%ht,                                   &
                           grid%t_max_p, ngrid%t_max_p,                         &
                           grid%ght_max_p, ngrid%ght_max_p,                     &
                           grid%max_p, ngrid%max_p,                             &
                           grid%t_min_p, ngrid%t_min_p,                         &
                           grid%ght_min_p, ngrid%ght_min_p,                     &
                           grid%min_p, ngrid%min_p,                             &
                           ngrid%znu, ngrid%p_top                               )

      RETURN
   END SUBROUTINE interp_domain_em_small_part2

!------------------------------------------------------------------

   SUBROUTINE feedback_nest_prep ( grid, config_flags    &
!
#include "dummy_new_args.inc"
!
)
      USE module_state_description
      USE module_domain, ONLY : domain, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask !, &
                             !push_communicators_for_domain, pop_communicators_for_domain
      USE module_comm_nesting_dm, ONLY : halo_interp_up_sub
      IMPLICIT NONE
!
      TYPE(domain), TARGET :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE (grid_config_rec_type) :: config_flags ! configureation flags, has vertical dim of
                                                  ! soil temp, moisture, etc., has vertical dim
                                                  ! of soil categories
#include "dummy_new_decl.inc"

      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER       :: idum1, idum2


      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

    IF ( grid%active_this_task ) THEN
      CALL push_communicators_for_domain( grid%id )

#ifdef DM_PARALLEL
#include "HALO_INTERP_UP.inc"
#endif

      CALL pop_communicators_for_domain
    END IF

   END SUBROUTINE feedback_nest_prep

!------------------------------------------------------------------

#endif
