!#define ONEWAY

SUBROUTINE med_force_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   IMPLICIT NONE
   TYPE(domain), POINTER :: parent_grid , nested_grid
   TYPE(domain), POINTER :: grid
   INTEGER nlev, msize
   TYPE (grid_config_rec_type)            :: config_flags
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

   INTERFACE
#if (EM_CORE == 1)
      SUBROUTINE interp_domain_em_part1 ( grid, intermediate_grid, config_flags ,  &
!
#  include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: intermediate_grid
         TYPE (grid_config_rec_type)            :: config_flags
#  include <em_dummy_decl.inc>
      END SUBROUTINE interp_domain_em_part1

      SUBROUTINE force_domain_em_part2 ( grid, nested_grid, config_flags ,  &
!
#  include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags
#  include <em_dummy_decl.inc>
      END SUBROUTINE force_domain_em_part2

      SUBROUTINE couple_or_uncouple_em ( grid, config_flags , couple,  &
!
#  include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
!         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), INTENT(INOUT) :: grid
         TYPE (grid_config_rec_type)            :: config_flags
         LOGICAL, INTENT(   IN) :: couple
#  include <em_dummy_decl.inc>
      END SUBROUTINE couple_or_uncouple_em

#endif
#if (NMM_CORE == 1)
#endif
#if (COAMPS_CORE == 1)
#endif
   END INTERFACE

   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )

   grid => parent_grid
#include "deref_kludge.h"

#ifndef DM_PARALLEL
! non-parallel not suported yet
#else

   IF ( .FALSE. ) THEN

#if (EM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM ) THEN

# ifdef ONEWAYBIT4BIT

! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
! vvvvvvvvvv EM PART 1 BFB (bug) vvvvvvvvvv
     grid => parent_grid
#  include "deref_kludge.h"
     CALL interp_domain_em_part1 ( grid , nested_grid%intermediate_grid, config_flags ,  &
!
#  include "em_actual_args.inc"
!
                 )

     grid => nested_grid%intermediate_grid
#  include "deref_kludge.h"
     CALL couple_or_uncouple_em ( grid , config_flags ,  .true., &
!
#  include "em_actual_args.inc"
!
                 )
     grid => nested_grid
#  include "deref_kludge.h"
     CALL couple_or_uncouple_em ( nested_grid , config_flags ,  .true., &
!
#  include "em_actual_args.inc"
!
                 )
! ^^^^^^^^^^ END EM PART 1 BFB (bug) ^^^^^^
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# else

! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
! vvvvvvvvvv EM PART 1 NON-BFB (works) vvvvvvvvvv

     CALL wrf_debug( 1, 'DAVE orig non-bit4bit' )

     ! couple parent domain
     grid => parent_grid
     CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
#  include "deref_kludge.h"
     CALL couple_or_uncouple_em ( grid , config_flags ,  .true., &
!
#  include "em_actual_args.inc"
!
                 )

     ! on restart do not force the nest the first time since it has already been forced
     ! prior to the writing of the restart file
     IF ( .NOT. ( config_flags%restart .AND. nested_grid%first_force ) ) THEN

        ! couple nested domain
        grid => nested_grid
        CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
#include "deref_kludge.h"
        CALL couple_or_uncouple_em ( nested_grid , config_flags ,  .true., &
!
#  include "em_actual_args.inc"
!
                 )

        ! perform first part: transfer data from parent to intermediate domain
        ! at the same resolution but on the same decomposition as the nest
        ! note that this will involve communication on multiple DM procs
        grid => parent_grid
        CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
#  include "deref_kludge.h"
        CALL interp_domain_em_part1 ( grid , nested_grid%intermediate_grid, config_flags ,  &
!
#  include "em_actual_args.inc"
!
                 )
     ENDIF ! not restart and first force

! ^^^^^^^^^^ END EM PART 1 NON-BFB (works) ^^^^^^
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# endif
#endif
   ENDIF


   grid => nested_grid%intermediate_grid
#include "deref_kludge.h"

   IF ( .FALSE. ) THEN

#if (EM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM ) THEN

! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
! vvvvvvvvvvv  EM PART 2  vvvvvvvvvvv

     ! on restart do not force the nest the first time...
     IF ( .NOT. ( config_flags%restart .AND. nested_grid%first_force ) ) THEN

        ! perform 2nd part: run interpolation on the intermediate domain
        ! and compute the values for the nest boundaries
        ! note that this is all local (no communication)
        CALL force_domain_em_part2 ( grid, nested_grid, config_flags ,  &
!
# include "em_actual_args.inc"
!
                 )

        ! uncouple the nest
        grid => nested_grid
#  include "deref_kludge.h"
        CALL couple_or_uncouple_em( nested_grid , config_flags ,  .false. , &
!
#  include "em_actual_args.inc"
!
                 )
     ENDIF ! not restart and first_force
     
     ! uncouple the parent
#  ifdef ONEWAYBIT4BIT
     grid => nested_grid%intermediate_grid
#  else
     grid => parent_grid
#  endif
#  include "deref_kludge.h"
     CALL couple_or_uncouple_em( grid , config_flags ,  .false. , &
!
#  include "em_actual_args.inc"
!
                 )

! ^^^^^^^^^^^  END EM PART 2  ^^^^^^^
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# endif

# if (NMM_CORE == 1)
# endif
# if (COAMPS_CORE == 1)
# endif

   ENDIF

! DM_PARALLEL
#endif

   IF ( nested_grid%first_force ) THEN
      nested_grid%first_force = .FALSE.
   ENDIF

   nested_grid%dtbc = 0.

   RETURN
END SUBROUTINE med_force_domain


