!
!WRF:MEDIATION_LAYER:NESTING
!
SUBROUTINE med_force_domain ( parent_grid , nested_grid )
#ifdef NMM_FIND_LOAD_IMBALANCE
   USE module_timing, only: now_time
#endif
   USE module_domain
   USE module_configure
   USE module_intermediate_nmm
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
#ifdef NMM_FIND_LOAD_IMBALANCE
   REAL(kind=8), save :: total_time(40)=0
   REAL(kind=8) :: this_time
   character*255 :: message
#endif
#if !defined(MAC_KLUDGE)
   TYPE (grid_config_rec_type)            :: config_flags
#endif

! ----------------------------------------------------------
! ------------------------------------------------------
! Interface blocks
! ------------------------------------------------------
   INTERFACE
! ------------------------------------------------------
!    Interface definitions for EM CORE
! ------------------------------------------------------
#if (EM_CORE == 1)
#if !defined(MAC_KLUDGE)
! ------------------------------------------------------
!    These routines are supplied by module_dm.F from the
!    external communication package (e.g. external/RSL)
! ------------------------------------------------------
      SUBROUTINE interp_domain_em_part1 ( grid, intermediate_grid, ngrid, config_flags   &
!
#        include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: intermediate_grid
         TYPE(domain), POINTER :: ngrid
         TYPE (grid_config_rec_type)            :: config_flags
#        include <dummy_new_decl.inc>
      END SUBROUTINE interp_domain_em_part1

      SUBROUTINE force_domain_em_part2 ( grid, nested_grid, parent_grid, config_flags   &
!
#        include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
	 TYPE(domain), POINTER :: parent_grid   ! KAL added for vertical nesting
         TYPE (grid_config_rec_type)            :: config_flags
#        include <dummy_new_decl.inc>
      END SUBROUTINE force_domain_em_part2

! ----------------------------------------------------------
!    This routine is supplied by dyn_em/couple_or_uncouple_em.F
! ----------------------------------------------------------
      SUBROUTINE couple_or_uncouple_em ( grid, config_flags , couple  &
!
#        include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), INTENT(INOUT)            :: grid
         TYPE (grid_config_rec_type)            :: config_flags
         LOGICAL, INTENT(   IN) :: couple
#        include <dummy_new_decl.inc>
      END SUBROUTINE couple_or_uncouple_em
#endif
#endif
! ----------------------------------------------------------
!    Interface definitions for NMM (placeholder)
! ----------------------------------------------------------
#if (NMM_CORE == 1 && NMM_NEST ==1)
!=======================================================================
!  Added for the NMM core. This is gopal's doing.
!=======================================================================

      SUBROUTINE force_domain_nmm_part1 ( grid, intermediate_grid, ngrid, config_flags    &
!
# include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: intermediate_grid
         TYPE(domain), POINTER :: ngrid
         TYPE (grid_config_rec_type)            :: config_flags
# include <dummy_new_decl.inc>
      END SUBROUTINE force_domain_nmm_part1

      SUBROUTINE before_interp_halos_nmm(grid,config_flags &
!
#include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE MODULE_CONFIGURE
         TYPE(domain), POINTER :: grid
         TYPE (grid_config_rec_type)            :: config_flags
#  include <dummy_new_decl.inc>
       END SUBROUTINE before_interp_halos_nmm

      SUBROUTINE force_domain_nmm_part2 ( grid, nested_grid, config_flags    &
!
# include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags

# include <dummy_new_decl.inc>
      END SUBROUTINE force_domain_nmm_part2
!=======================================================================
!  End of gopal's doing.
!=======================================================================
#endif
! ----------------------------------------------------------
!    Interface definitions for COAMPS (placeholder)
! ----------------------------------------------------------
#if (COAMPS_CORE == 1)
#endif
   END INTERFACE
! ----------------------------------------------------------
! End of Interface blocks
! ----------------------------------------------------------
! ----------------------------------------------------------
! ----------------------------------------------------------
! Executable code
! ----------------------------------------------------------
#ifdef NMM_FIND_LOAD_IMBALANCE
this_time=now_time()
#endif
! ----------------------------------------------------------
!    Forcing calls for EM CORE.
! ----------------------------------------------------------
#if (EM_CORE == 1 && defined( DM_PARALLEL ))
# if !defined(MAC_KLUDGE)
   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )

   grid => nested_grid%intermediate_grid
#  if (defined(MOVE_NESTS) || (!defined(SGIALTIX) && (!defined(FUJITSU_FX10))))
   CALL alloc_space_field ( grid, grid%id , 1 , 2 ,  .TRUE. ,    &
                            grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                            grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                            grid%sp31,  grid%ep31,  grid%sp32,  grid%ep32,  grid%sp33,  grid%ep33, &
                            grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x,&
                            grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y,&
                            grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                            grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
     )
#  endif

   ! couple parent domain
   grid => parent_grid
   ! swich config_flags to point to parent rconfig info
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL couple_or_uncouple_em ( grid , config_flags ,  .true. &
!
#         include "actual_new_args.inc"
!
                                )
   ! couple nested domain
   grid => nested_grid
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL couple_or_uncouple_em ( grid , config_flags ,  .true. &
!
#         include "actual_new_args.inc"
!
                                   )
   ! perform first part: transfer data from parent to intermediate domain
   ! at the same resolution but on the same decomposition as the nest
   ! note that this will involve communication on multiple DM procs
   grid => parent_grid
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   !
   ! Added following line to handle adaptive time step.  This should probably
   !   go somewhere else, but I'm not sure where.
   !   
   ! T. Hutchinson, WSI  1/23/07
   !
   nested_grid%intermediate_grid%dt = grid%dt

   CALL interp_domain_em_part1 ( grid , nested_grid%intermediate_grid, nested_grid, config_flags  &
!
#         include "actual_new_args.inc"
!
                                    )
   grid => nested_grid%intermediate_grid
      ! perform 2nd part: run interpolation on the intermediate domain
      ! and compute the values for the nest boundaries
      ! note that this is all local (no communication)
   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
   CALL force_domain_em_part2 ( grid, nested_grid, parent_grid, config_flags   &
!
#          include "actual_new_args.inc"
!
                                   )
   ! uncouple the nest
   grid => nested_grid
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL couple_or_uncouple_em ( grid , config_flags ,  .false.  &
!
#          include "actual_new_args.inc"
!
                                   )
   ! uncouple the parent
   grid => parent_grid
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL couple_or_uncouple_em ( grid , config_flags ,  .false.  &
!
#          include "actual_new_args.inc"
!
                                )
   IF ( nested_grid%first_force ) THEN
      nested_grid%first_force = .FALSE.
   ENDIF
   nested_grid%dtbc = 0.
!
   grid => nested_grid%intermediate_grid
#  if (defined(MOVE_NESTS) || (!defined(SGIALTIX) && (!defined(FUJITSU_FX10))))
   CALL dealloc_space_field ( grid )
#  endif
# endif
#endif
! ------------------------------------------------------
!    End of Forcing calls for EM CORE.
! ------------------------------------------------------
! ------------------------------------------------------
! ------------------------------------------------------
!    Forcing calls for NMM. (Placeholder)
! ------------------------------------------------------
# if (NMM_CORE == 1 && NMM_NEST == 1)
!=======================================================================
!  Added for the NMM core. This is gopal's doing.
!=======================================================================

   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )

   grid => nested_grid%intermediate_grid
!dusan orig    CALL alloc_space_field ( grid, grid%id , 1 , 2 ,  .TRUE. ,    &
#  if (defined(MOVE_NESTS) || (!defined(SGIALTIX) && (!defined(FUJITSU_FX10))))
   CALL ensure_space_field &
                          ( grid, grid%id , 1 , 3 ,  .FALSE. ,    &
                            grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                            grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                            grid%sp31,  grid%ep31,  grid%sp32,  grid%ep32,  grid%sp33,  grid%ep33, &
                            grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x,&
                            grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y,&
                            grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                            grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
      )
#endif
   nested_grid%intermediate_grid%interp_mp=parent_grid%interp_mp .or. nested_grid%interp_mp
#if (HWRF == 1)
  nested_grid%intermediate_grid%pdyn_parent_age=parent_grid%pdyn_parent_age
  nested_grid%intermediate_grid%pdyn_smooth_age=parent_grid%pdyn_smooth_age
#endif


    ! couple parent domain
 !   grid => parent_grid
    ! swich config_flags to point to parent rconfig info
 !   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

    ! on restart do not force the nest the first time since it has already been forced
    ! prior to the writing of the restart file
!    IF ( .NOT. ( config_flags%restart .AND. nested_grid%first_force ) ) THEN
       ! couple nested domain
       grid => nested_grid
       CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
       ! Apply halos on weights and indices:
    call before_interp_halos_nmm(grid,config_flags &
!
#   include "actual_new_args.inc"
!
                                )

   ! Copy interpolation information from parent grid to intermediate grid:
 !  grid => parent_grid
 !  call parent_to_inter_part1(parent_grid, nested_grid%intermediate_grid, &
 !                             nested_grid, config_flags)

 !  grid => nested_grid%intermediate_grid
 !  call parent_to_inter_part2(nested_grid%intermediate_grid, config_flags)

   ! Transfer remaining information from parent to intermediate grid:
   grid => parent_grid
   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL force_domain_nmm_part1 ( grid , nested_grid%intermediate_grid,  &
                                 nested_grid, config_flags    &
!
#         include "actual_new_args.inc"
!
                                     )
    grid => nested_grid%intermediate_grid
    IF ( .NOT. ( config_flags%restart .AND. nested_grid%first_force ) ) THEN
       ! Finish transfering information from parent to intermediate
       ! grid, then run interpolation on the intermediate domain and
       ! compute the values for the nest boundaries.
       CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
       CALL force_domain_nmm_part2 ( grid, nested_grid, config_flags    &
!
#         include "actual_new_args.inc"
!
)
    ENDIF ! not restart and first_force

    IF ( nested_grid%first_force ) THEN
       nested_grid%first_force = .FALSE.
    ENDIF
    nested_grid%dtbc = 0.
!
    grid => nested_grid%intermediate_grid
!=======================================================================
!  End of gopal's doing.
!=======================================================================
# endif
! ------------------------------------------------------
!    End of Forcing calls for NMM.
! ------------------------------------------------------
! ------------------------------------------------------
! ------------------------------------------------------
!    Forcing calls for COAMPS. (Placeholder)
! ------------------------------------------------------
# if (COAMPS_CORE == 1)
# endif
! ------------------------------------------------------
!    End of Forcing calls for COAMPS.
! ------------------------------------------------------
#ifdef NMM_FIND_LOAD_IMBALANCE
   this_time=now_time()-this_time
   total_time(parent_grid%id)=total_time(parent_grid%id)+this_time
30 format('med_force_domain for grid ',I0,' to grid ',I0,': ',F12.6,'s; running total: ',F12.6,'s')
   write(message,30) parent_grid%id,nested_grid%id,this_time,total_time(parent_grid%id)
   call wrf_debug(1,message)
#endif
   RETURN
END SUBROUTINE med_force_domain


