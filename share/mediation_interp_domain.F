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
   END INTERFACE
! ----------------------------------------------------------
! End of Interface blocks
! ----------------------------------------------------------
! ----------------------------------------------------------
! ----------------------------------------------------------
! Executable code
! ----------------------------------------------------------
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

