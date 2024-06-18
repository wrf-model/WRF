#if ( EM_CORE == 1 && DA_CORE != 1 )

!------------------------------------------------------------------

   SUBROUTINE feedback_domain_em_part2 ( grid, intermediate_grid, ngrid , config_flags    &
!
#include "dummy_new_args.inc"
!
                 )
      USE module_state_description
      USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_grid
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec
      USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
                            ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,  &
                            nest_pes_x, nest_pes_y,                                         &
                            intercomm_active, nest_task_offsets,                    &
                            mpi_comm_to_mom, mpi_comm_to_kid, which_kid !,            &
                             !push_communicators_for_domain, pop_communicators_for_domain

      USE module_comm_nesting_dm, ONLY : halo_interp_up_sub
      USE module_utility
      IMPLICIT NONE

!
      TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
      TYPE(domain), POINTER :: intermediate_grid
      TYPE(domain), POINTER :: ngrid
      TYPE(domain), POINTER :: parent_grid

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
      INTEGER       ::          xids, xide, xjds, xjde, xkds, xkde,    &
                                xims, xime, xjms, xjme, xkms, xkme,    &
                                xips, xipe, xjps, xjpe, xkps, xkpe
      INTEGER       ::          ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe

      INTEGER idim1,idim2,idim3,idim4,idim5,idim6,idim7

      INTEGER icoord, jcoord, idim_cd, jdim_cd
      INTEGER local_comm, myproc, nproc, ioffset
      INTEGER iparstrt, jparstrt, sw, thisdomain_max_halo_width
      REAL    nest_influence

      character*256 :: timestr
      integer ierr

      LOGICAL, EXTERNAL  :: cd_feedback_mask

!cyl: add variables for trajectory
      integer tjk

! On entry to this routine,
!  "grid" refers to the parent domain
!  "intermediate_grid" refers to local copy of parent domain that overlies this patch of nest
!  "ngrid" refers to the nest, which is only needed for smoothing on the parent because
!          the nest feedback data has already been transferred during em_nest_feedbackup_interp
!          in part1, above.
! The way these settings c and n dimensions are set, below, looks backwards but from the point
! of view of the RSL routine rsl_lite_to_parent_info(), call to which is included by
! em_nest_feedbackup_pack, the "n" domain represents the parent domain and the "c" domain
! represents the intermediate domain. The backwards lookingness should be fixed in the gen_comms.c
! registry routine that accompanies RSL_LITE but, just as it's sometimes easier to put up a road
! sign that says "DIP" than fix the dip,  at this point it was easier just to write this comment. JM
!
      nest_influence = 1.

      CALL domain_clock_get( grid, current_timestr=timestr )

      CALL get_ijk_from_grid (  intermediate_grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )
      CALL get_ijk_from_grid (  grid ,              &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )
      CALL get_ijk_from_grid (  ngrid ,              &
                                xids, xide, xjds, xjde, xkds, xkde,    &
                                xims, xime, xjms, xjme, xkms, xkme,    &
                                xips, xipe, xjps, xjpe, xkps, xkpe    )

      ips_save = ngrid%i_parent_start   ! used in feedback_domain_em_part2 below
      jps_save = ngrid%j_parent_start
      ipe_save = ngrid%i_parent_start + (xide-xids+1) / ngrid%parent_grid_ratio - 1
      jpe_save = ngrid%j_parent_start + (xjde-xjds+1) / ngrid%parent_grid_ratio - 1




IF ( ngrid%active_this_task ) THEN
!cyl add this for trajectory
    CALL push_communicators_for_domain( ngrid%id )

    do tjk = 1,config_flags%num_traj
     if (ngrid%traj_long(tjk) .eq. -9999.0) then
!       print*,'n=-9999',tjk
        ngrid%traj_long(tjk)=grid%traj_long(tjk)
        ngrid%traj_k(tjk)=grid%traj_k(tjk)
     else
!       print*,'p!=-9999',tjk
        grid%traj_long(tjk)=ngrid%traj_long(tjk)
        grid%traj_k(tjk)=ngrid%traj_k(tjk)
     endif
     if (ngrid%traj_lat(tjk) .eq. -9999.0) then
         ngrid%traj_lat(tjk)=grid%traj_lat(tjk)
         ngrid%traj_k(tjk)=grid%traj_k(tjk)
     else
         grid%traj_lat(tjk)=ngrid%traj_lat(tjk)
         grid%traj_k(tjk)=ngrid%traj_k(tjk)
     endif
    enddo
!endcyl

      CALL nl_get_i_parent_start ( intermediate_grid%id, iparstrt )
      CALL nl_get_j_parent_start ( intermediate_grid%id, jparstrt )
      CALL nl_get_shw            ( intermediate_grid%id, sw )
      icoord =    iparstrt - sw
      jcoord =    jparstrt - sw
      idim_cd = cide - cids + 1
      jdim_cd = cjde - cjds + 1

      nlev  = ckde - ckds + 1

      CALL get_dm_max_halo_width ( grid%id , thisdomain_max_halo_width )

      parent_grid => grid
      grid => ngrid
#include "nest_feedbackup_pack.inc"
      grid => parent_grid
    CALL pop_communicators_for_domain

END IF

!      CALL wrf_get_dm_communicator ( local_comm )
!      CALL wrf_get_myproc( myproc )
!      CALL wrf_get_nproc( nproc )

      ! determine which communicator and offset to use
      IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
        local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
        ioffset = nest_task_offsets(ngrid%id)
      ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
        local_comm = mpi_comm_to_mom( ngrid%id )
        ioffset = nest_task_offsets(ngrid%id)
      END IF

      IF ( grid%active_this_task .OR. ngrid%active_this_task ) THEN
#ifndef STUBMPI
        CALL mpi_comm_rank(local_comm,myproc,ierr)
        CALL mpi_comm_size(local_comm,nproc,ierr)
#endif
!call tracebackqq()
        CALL rsl_lite_merge_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                          nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                          ioffset, local_comm )
      END IF

IF ( grid%active_this_task ) THEN
    CALL push_communicators_for_domain( grid%id )


#define NEST_INFLUENCE(A,B) A = B
#include "nest_feedbackup_unpack.inc"

      ! smooth coarse grid
      CALL get_ijk_from_grid (  ngrid,                           &
                                nids, nide, njds, njde, nkds, nkde,    &
                                nims, nime, njms, njme, nkms, nkme,    &
                                nips, nipe, njps, njpe, nkps, nkpe    )
      CALL get_ijk_from_grid (  grid ,              &
                                ids, ide, jds, jde, kds, kde,    &
                                ims, ime, jms, jme, kms, kme,    &
                                ips, ipe, jps, jpe, kps, kpe    )

#include "HALO_INTERP_UP.inc"

      CALL get_ijk_from_grid (  grid ,                   &
                                cids, cide, cjds, cjde, ckds, ckde,    &
                                cims, cime, cjms, cjme, ckms, ckme,    &
                                cips, cipe, cjps, cjpe, ckps, ckpe    )

#include "nest_feedbackup_smooth.inc"

    CALL pop_communicators_for_domain
END IF

      RETURN
   END SUBROUTINE feedback_domain_em_part2
#endif
