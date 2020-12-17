!
!WRF:MEDIATION_LAYER:NESTING
!
SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   USE module_timing

   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags
#ifdef NMM_FIND_LOAD_IMBALANCE
   REAL(kind=8), save :: total_time(40)=0
   REAL(kind=8) :: this_time
   character*255 :: message
#endif
! ----------------------------------------------------------
! ----------------------------------------------------------
! Interface blocks
! ----------------------------------------------------------
   INTERFACE
! ----------------------------------------------------------
!    Interface definitions for EM CORE
! ----------------------------------------------------------
#if (EM_CORE == 1)
! ----------------------------------------------------------
!    These routines are supplied by module_dm.F from the 
!    external communication package (e.g. external/RSL)
! ----------------------------------------------------------
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
#        include "dummy_new_decl.inc"
      END SUBROUTINE interp_domain_em_part1

      SUBROUTINE interp_domain_em_part2 ( grid, nested_grid, parent_grid, config_flags   &
!
#        include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE(domain), POINTER :: parent_grid   !KAL added for vertical nesting
         TYPE (grid_config_rec_type)            :: config_flags
#        include "dummy_new_decl.inc"
      END SUBROUTINE interp_domain_em_part2
#endif
! ----------------------------------------------------------
!    Interface definitions for NMM (placeholder)
! ----------------------------------------------------------
#if (NMM_CORE == 1 && NMM_NEST == 1)
!=======================================================================
!  Added for the NMM core. This is gopal's doing.
!=======================================================================

      SUBROUTINE interp_domain_nmm_part1 ( grid, intermediate_grid, ngrid, config_flags    &
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
# include "dummy_new_decl.inc"
      END SUBROUTINE interp_domain_nmm_part1

      SUBROUTINE interp_domain_nmm_part2 ( grid, nested_grid, config_flags    &
!
# include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags
# include "dummy_new_decl.inc"
      END SUBROUTINE interp_domain_nmm_part2

!=======================================================================
!  End of gopal's doing.
!=======================================================================
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
!    Interpolation calls for EM CORE.  The called 
!    routines below are supplied by module_dm.F
!    from the external communications package (e.g. RSL)
! ----------------------------------------------------------
#if (EM_CORE == 1 && defined( DM_PARALLEL ))

  CALL wrf_dm_nestexchange_init

  CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
  grid => nested_grid%intermediate_grid
#  if (defined(MOVE_NESTS) || ((!defined(KEEP_INT_AROUND)) && (!defined(SGIALTIX)) && (!defined(FUJITSU_FX10))))

    CALL alloc_space_field ( grid, grid%id , 1 , 2 , .TRUE. , nested_grid%active_this_task,  &
                           grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                           grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                           grid%sp31,  grid%ep31,  grid%sp32,  grid%ep32,  grid%sp33,  grid%ep33, &
                           grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x,&
                           grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y,&
                           grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                           grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
    )
# endif

  grid => parent_grid

  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  CALL interp_domain_em_part1 ( grid , nested_grid%intermediate_grid, nested_grid, config_flags   &
!
#     include "actual_new_args.inc"
!
                                )
  IF ( nested_grid%active_this_task ) THEN
  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  
  CALL interp_domain_em_part2 ( grid, nested_grid, parent_grid, config_flags   &
!
#     include "actual_new_args.inc"
!
                                )
  ENDIF

  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
# if (defined(MOVE_NESTS) || ((!defined(KEEP_INT_AROUND)) && (!defined(SGIALTIX)) && (!defined(FUJITSU_FX10))))
  IF ( grid%active_this_task ) THEN
    CALL dealloc_space_field ( grid )
  ENDIF
# endif
#endif
! ------------------------------------------------------
!    End of Interpolation calls for EM CORE.
! ------------------------------------------------------
! ------------------------------------------------------
! ------------------------------------------------------
!    Interpolation calls for NMM. (Placeholder)
! ------------------------------------------------------
#if (NMM_CORE == 1 && NMM_NEST == 1)
!=======================================================================
!  Added for the NMM core. This is gopal's doing.
!=======================================================================
!
  CALL wrf_dm_nestexchange_init
  CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
  grid => nested_grid%intermediate_grid
#  if (defined(MOVE_NESTS) || ((!defined(KEEP_INT_AROUND)) && (!defined(SGIALTIX)) && (!defined(FUJITSU_FX10))))
    CALL ensure_space_field &
                         ( grid, grid%id , 1 , 2 , .TRUE. , nested_grid%active_this_task,   &
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

  grid => parent_grid

  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

  CALL interp_domain_nmm_part1 ( grid , nested_grid%intermediate_grid, nested_grid, config_flags    &
!
#     include "actual_new_args.inc"
!
                                )
  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  IF ( nested_grid%active_this_task ) THEN

  CALL interp_domain_nmm_part2 ( grid, nested_grid, config_flags    &
!
#     include "actual_new_args.inc"
!
                                )

  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  ENDIF
! ------------------------------------------------------------
!    End of gopal's doing
! ------------------------------------------------------------
#endif
! ------------------------------------------------------
!    End of Interpolation calls for NMM.
! ------------------------------------------------------
   RETURN
END SUBROUTINE med_interp_domain

!
SUBROUTINE med_interp_domain_small ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   USE module_timing
#if (defined(MOVE_NESTS) || ((!defined(KEEP_INT_AROUND)) && (!defined(SGIALTIX)) && (!defined(FUJITSU_FX10))))
   USE module_dm, ONLY : intercomm_active
#endif
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags

   INTERFACE
#if (EM_CORE == 1)
! ----------------------------------------------------------
      SUBROUTINE interp_domain_em_small_part1 ( grid, intermediate_grid, ngrid, config_flags   &
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
#        include "dummy_new_decl.inc"
      END SUBROUTINE interp_domain_em_small_part1

      SUBROUTINE interp_domain_em_small_part2 ( grid, nested_grid, config_flags   &
!
#        include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags
#        include "dummy_new_decl.inc"
      END SUBROUTINE interp_domain_em_small_part2
#endif
   END INTERFACE

! ----------------------------------------------------------
!    Interpolation calls for EM CORE.  The called 
!    routines below are supplied by module_dm.F
!    from the external communications package (e.g. RSL)
! ----------------------------------------------------------

#if (EM_CORE == 1 && defined( DM_PARALLEL ))
  CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
  grid => nested_grid%intermediate_grid
#  if (defined(MOVE_NESTS) || ((!defined(KEEP_INT_AROUND)) && (!defined(SGIALTIX)) && (!defined(FUJITSU_FX10))))

  CALL alloc_space_field ( grid, grid%id , 1 , 2 , .TRUE. , nested_grid%active_this_task,   &
                           grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                           grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                           grid%sp31,  grid%ep31,  grid%sp32,  grid%ep32,  grid%sp33,  grid%ep33, &
                           grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x,&
                           grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y,&
                           grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                           grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
    )
# endif

  grid => parent_grid

  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  CALL interp_domain_em_small_part1 ( grid , nested_grid%intermediate_grid, nested_grid, config_flags   &
!
#     include "actual_new_args.inc"
!
                                )
  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  CALL interp_domain_em_small_part2 ( grid, nested_grid, config_flags   &
!
#     include "actual_new_args.inc"
!
                                )

  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
#  if (defined(MOVE_NESTS) || ((!defined(KEEP_INT_AROUND)) && (!defined(SGIALTIX)) && (!defined(FUJITSU_FX10))))
  IF ( intercomm_active( grid%id ) ) THEN
  CALL dealloc_space_field ( grid )
  ENDIF
# endif
#endif
! ------------------------------------------------------
!    End of Interpolation calls for EM CORE.
! ------------------------------------------------------
call wrf_debug(0,'FILE: share/mediation_interp_domain.F  ROUTINE: med_interp_domain_small   CALLING: done ')
   RETURN
END SUBROUTINE med_interp_domain_small

