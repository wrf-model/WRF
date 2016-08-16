! MODULE module_intermediate_nmm

! This module contains routines that feed parent grid variables to the
! intermediate grid when doing up-interpolation.  This is needed by
! the new NMM interpolation routines, which require certain variables
! on the target domain in order to do log(P)-space vertical
! interpolation.
!
! This module is also used during forcing (parent->nest boundary) to
! copy variables to the intermediate domain that may not otherwise be
! copied by the forcing routines.
!
! Author: Samuel Trahan
!
! History:
!   Aug 2012 - written by Sam Trahan for up-interpolation
!   Sep 2012 - updated to also work with forcing (parent->nest bdy)

module module_intermediate_nmm
#if (NMM_CORE == 1 && NMM_NEST==1)
contains
  SUBROUTINE parent_to_inter_part1 ( grid, intermediate_grid, ngrid, config_flags )
    USE module_state_description
    USE module_domain, ONLY : domain, get_ijk_from_grid
    USE module_configure, ONLY : grid_config_rec_type
    USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
         ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width,                   &
         nest_pes_x, nest_pes_y,                                                          &
         intercomm_active, nest_task_offsets,                                             &
         mpi_comm_to_mom, mpi_comm_to_kid, which_kid!,                                     &
         !push_communicators_for_domain,pop_communicators_for_domain


    USE module_timing
    IMPLICIT NONE

    TYPE(domain), POINTER :: grid          
    TYPE(domain), POINTER :: intermediate_grid
    TYPE(domain), POINTER :: ngrid
    INTEGER nlev, msize
    INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k,ioffset,ierr
    INTEGER iparstrt,jparstrt,sw
    TYPE (grid_config_rec_type)            :: config_flags
    REAL xv(500)
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
    INTEGER local_comm, myproc, nproc
    INTEGER thisdomain_max_halo_width

!    CALL wrf_get_dm_communicator ( local_comm )
    CALL wrf_get_myproc( myproc )
    CALL wrf_get_nproc( nproc )

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

    CALL get_dm_max_halo_width ( ngrid%id , thisdomain_max_halo_width )
    CALL wrf_dm_nestexchange_init

    IF ( grid%active_this_task ) THEN
      msize = 5
      CALL rsl_lite_to_child_info( msize*4                               &
         ,cips,cipe,cjps,cjpe                               &
         ,iids,iide,ijds,ijde                               &
         ,nids,nide,njds,njde                               &
         ,pgr , sw                                          &
         ,nest_task_offsets(ngrid%id)                       &
         ,nest_pes_x(grid%id)                               &
         ,nest_pes_y(grid%id)                               &
         ,nest_pes_x(intermediate_grid%id)                  &
         ,nest_pes_y(intermediate_grid%id)                  &
         ,thisdomain_max_halo_width                         &
         ,icoord,jcoord                                     &
         ,idim_cd,jdim_cd                                   &
         ,pig,pjg,retval )
      DO while ( retval .eq. 1 )
       IF ( SIZE(grid%hres_fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          xv(1)=grid%hres_fis(pig,pjg)
          CALL rsl_lite_to_child_msg(4,xv)
       ENDIF
       IF ( SIZE(grid%sm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          xv(1)=grid%sm(pig,pjg)
          CALL rsl_lite_to_child_msg(4,xv)
       ENDIF
       IF ( SIZE(grid%pd) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          xv(1)=grid%pd(pig,pjg)
          CALL rsl_lite_to_child_msg(4,xv)
       ENDIF
       IF ( SIZE(grid%fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          xv(1)=grid%fis(pig,pjg)
          CALL rsl_lite_to_child_msg(4,xv)
       ENDIF
       CALL rsl_lite_to_child_info( msize*4                               &
            ,cips,cipe,cjps,cjpe                               &
            ,iids,iide,ijds,ijde                               &
            ,nids,nide,njds,njde                               &
            ,pgr , sw                                          &
            ,nest_task_offsets(ngrid%id)                       &
            ,nest_pes_x(grid%id)                               &
            ,nest_pes_y(grid%id)                               &
            ,nest_pes_x(intermediate_grid%id)                  &
            ,nest_pes_y(intermediate_grid%id)                  &
            ,thisdomain_max_halo_width                         &
            ,icoord,jcoord                                     &
            ,idim_cd,jdim_cd                                   &
            ,pig,pjg,retval )
      ENDDO
    ENDIF ! grid%active_this_task

    IF ( intercomm_active( grid%id ) ) THEN        ! I am parent
      local_comm = mpi_comm_to_kid( which_kid(ngrid%id), grid%id )
      ioffset = nest_task_offsets(ngrid%id)
    ELSE IF ( intercomm_active( ngrid%id ) ) THEN  ! I am nest
      local_comm = mpi_comm_to_mom( ngrid%id )
      ioffset = nest_task_offsets(ngrid%id)
    ENDIF

    IF ( grid%active_this_task .OR. ngrid%active_this_task ) THEN
#if defined(DM_PARALLEL) && !defined(STUBMPI)
      CALL mpi_comm_rank(local_comm,myproc,ierr)
      CALL mpi_comm_size(local_comm,nproc,ierr)
#endif
      CALL rsl_lite_bcast_msgs( myproc, nest_pes_x(grid%id)*nest_pes_y(grid%id),         &
                                          nest_pes_x(ngrid%id)*nest_pes_y(ngrid%id),       &
                                          ioffset, local_comm )
    ENDIF

    RETURN
  END SUBROUTINE parent_to_inter_part1

  SUBROUTINE parent_to_inter_part2 ( grid, ngrid, config_flags )
    USE module_state_description
    USE module_domain, ONLY : domain, get_ijk_from_grid
    USE module_configure, ONLY : grid_config_rec_type
    USE module_dm, ONLY : ntasks, ntasks_x, ntasks_y, itrace, local_communicator, mytask, &
         ipe_save, jpe_save, ips_save, jps_save, get_dm_max_halo_width!,                   &
         !push_communicators_for_domain,pop_communicators_for_domain

    USE module_comm_dm, ONLY : HALO_NMM_INT_UP_sub
    IMPLICIT NONE

    TYPE(domain), POINTER :: grid          
    TYPE(domain), POINTER :: cgrid
    TYPE(domain), POINTER :: ngrid

    INTEGER nlev, msize
    INTEGER i,j,pig,pjg,cm,cn,nig,njg,retval,k
    TYPE (grid_config_rec_type)            :: config_flags
    REAL xv(500)
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
    REAL  dummy_xs, dummy_xe, dummy_ys, dummy_ye

    integer myproc

    CALL get_ijk_from_grid (  grid ,                   &
         cids, cide, cjds, cjde, ckds, ckde,    &
         cims, cime, cjms, cjme, ckms, ckme,    &
         cips, cipe, cjps, cjpe, ckps, ckpe    )

IF ( ngrid%active_this_task ) THEN
    nlev  = ckde - ckds + 1 
    !write(0,*) 'IN parent_to_inter_part2'
    CALL rsl_lite_from_parent_info(pig,pjg,retval)
    DO while ( retval .eq. 1 )
    !write(0,*) 'top of loop'
       IF ( SIZE(grid%hres_fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          CALL rsl_lite_from_parent_msg(4,xv)
          grid%hres_fis(pig,pjg) = xv(1)
       ENDIF
       !write(0,*)'do sm'
       IF ( SIZE(grid%sm) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          CALL rsl_lite_from_parent_msg(4,xv)
          grid%sm(pig,pjg) = xv(1)
       ENDIF
       !write(0,*)'do pd'
       IF ( SIZE(grid%pd) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          CALL rsl_lite_from_parent_msg(4,xv)
          grid%pd(pig,pjg) = xv(1)
       ENDIF
       !write(0,*)'do fis'
       IF ( SIZE(grid%fis) .GT. 1 ) THEN ! okay for intermediate_grid too. see comment in gen_comms.c
          CALL rsl_lite_from_parent_msg(4,xv)
          grid%fis(pig,pjg) = xv(1)
       ENDIF
       !write(0,*) 'call rsl_lite_from_parent_info'
       CALL rsl_lite_from_parent_info(pig,pjg,retval)
       !write(0,*) 'back with retval=',retval
    ENDDO


    !write(0,*) 'out of loop'

    CALL get_ijk_from_grid (  grid ,              &
         ids, ide, jds, jde, kds, kde,    &
         ims, ime, jms, jme, kms, kme,    &
         ips, ipe, jps, jpe, kps, kpe    )

    CALL push_communicators_for_domain( grid%id )
#include "HALO_NMM_INT_UP.inc"
    CALL pop_communicators_for_domain

ENDIF
    CALL wrf_dm_nestexchange_init
    RETURN
  END SUBROUTINE parent_to_inter_part2
#endif
end module module_intermediate_nmm
