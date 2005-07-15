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
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y

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
      SUBROUTINE interp_domain_em_part1 ( grid, intermediate_grid, ngrid, config_flags ,  &
!
#        include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: intermediate_grid
         TYPE(domain), POINTER :: ngrid
         TYPE (grid_config_rec_type)            :: config_flags
#        include <em_dummy_decl.inc>
      END SUBROUTINE interp_domain_em_part1

      SUBROUTINE interp_domain_em_part2 ( grid, nested_grid, config_flags ,  &
!
#        include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags
#        include <em_dummy_decl.inc>
      END SUBROUTINE interp_domain_em_part2
#endif
! ----------------------------------------------------------
!    Interface definitions for NMM (placeholder)
! ----------------------------------------------------------
#if (NMM_CORE == 1)
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
  IF ( config_flags%dyn_opt == DYN_EM ) THEN

    grid => nested_grid%intermediate_grid
    CALL alloc_space_field ( grid, grid%id , 1 , 2 , .TRUE. ,   &
                             grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                             grid%sm31,  grid%em31,  grid%sm32,  grid%em32,  grid%sm33,  grid%em33, &
                             grid%sm31x, grid%em31x, grid%sm32x, grid%em32x, grid%sm33x, grid%em33x, &   ! x-xpose
                             grid%sm31y, grid%em31y, grid%sm32y, grid%em32y, grid%sm33y, grid%em33y  &   ! y-xpose
      )

    grid => parent_grid

#   include "deref_kludge.h"


    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
    CALL interp_domain_em_part1 ( grid , nested_grid%intermediate_grid, nested_grid, config_flags ,  &
!
#     include "em_actual_args.inc"
!
                                )
    grid => nested_grid%intermediate_grid
    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
#   include "deref_kludge.h"
    CALL interp_domain_em_part2 ( grid, nested_grid, config_flags ,  &
!
#     include "em_actual_args.inc"
!
                                )

    grid => nested_grid%intermediate_grid
    CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
    CALL dealloc_space_field ( grid )

  ENDIF
#endif
! ------------------------------------------------------
!    End of Interpolation calls for EM CORE.
! ------------------------------------------------------
! ------------------------------------------------------
! ------------------------------------------------------
!    Interpolation calls for NMM. (Placeholder)
! ------------------------------------------------------
#if (NMM_CORE == 1)
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


