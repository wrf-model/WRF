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
#        include <dummy_new_decl.inc>
      END SUBROUTINE interp_domain_em_part1

      SUBROUTINE interp_domain_em_part2 ( grid, nested_grid, config_flags   &
!
#        include "dummy_new_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags
#        include <dummy_new_decl.inc>
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
# include <dummy_new_decl.inc>
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
# include <dummy_new_decl.inc>
      END SUBROUTINE interp_domain_nmm_part2

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
! ----------------------------------------------------------
!    Interpolation calls for EM CORE.  The called 
!    routines below are supplied by module_dm.F
!    from the external communications package (e.g. RSL)
! ----------------------------------------------------------
#if (EM_CORE == 1 && defined( DM_PARALLEL ))
  CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
  grid => nested_grid%intermediate_grid
# if defined(MOVE_NESTS) || (!defined(SGIALTIX))

  CALL alloc_space_field ( grid, grid%id , 1 , 2 , .TRUE. ,   &
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
  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
  CALL interp_domain_em_part2 ( grid, nested_grid, config_flags   &
!
#     include "actual_new_args.inc"
!
                                )

  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
# if defined(MOVE_NESTS) || (!defined(SGIALTIX))
  CALL dealloc_space_field ( grid )
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
  CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )
  grid => nested_grid%intermediate_grid
#if defined(MOVE_NESTS) || (!defined(SGIALTIX))
  CALL alloc_space_field ( grid, grid%id , 1 , 2 , .TRUE. ,   &
                           grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                           grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                           grid%sp31,  grid%ep31,  grid%sp32,  grid%ep32,  grid%sp33,  grid%ep33, &
                           grid%sp31x, grid%ep31x, grid%sp32x, grid%ep32x, grid%sp33x, grid%ep33x,&
                           grid%sp31y, grid%ep31y, grid%sp32y, grid%ep32y, grid%sp33y, grid%ep33y,&
                           grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                           grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
      )
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

  CALL interp_domain_nmm_part2 ( grid, nested_grid, config_flags    &
!
#     include "actual_new_args.inc"
!
                                )

  grid => nested_grid%intermediate_grid
  CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
#if defined(MOVE_NESTS) || (!defined(SGIALTIX))
  CALL dealloc_space_field ( grid )
#endif

! ------------------------------------------------------------
!    End of gopal's doing
! ------------------------------------------------------------
#endif
! ------------------------------------------------------
!    End of Interpolation calls for NMM.
! ------------------------------------------------------
! ------------------------------------------------------
! ------------------------------------------------------
!    Interpolation calls for COAMPS. (Placeholder)
! ------------------------------------------------------
#if (COAMPS_CORE == 1)
#endif
! ------------------------------------------------------
!    End of Interpolation calls for COAMPS.
! ------------------------------------------------------
   RETURN
END SUBROUTINE med_interp_domain


