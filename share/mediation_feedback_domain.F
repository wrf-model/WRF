SUBROUTINE med_feedback_domain ( parent_grid , nested_grid )
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
      SUBROUTINE feedback_domain_em_part1 ( grid, nested_grid, config_flags ,  &
!
# include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags
# include <em_dummy_decl.inc>
      END SUBROUTINE feedback_domain_em_part1


      SUBROUTINE feedback_domain_em_part2 ( grid, intermediate_grid , nested_grid, config_flags ,  &
!
# include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: intermediate_grid
         TYPE(domain), POINTER :: nested_grid
         TYPE (grid_config_rec_type)            :: config_flags
# include <em_dummy_decl.inc>
      END SUBROUTINE feedback_domain_em_part2
#endif

#if (NMM_CORE == 1 )
#endif
#if (COAMPS_CORE == 1 )
#endif

   END INTERFACE

   CALL model_to_grid_config_rec ( nested_grid%id , model_config_rec , config_flags )

   grid => nested_grid%intermediate_grid
#include "deref_kludge.h"

#ifndef DM_PARALLEL

write(0,*)' W A R N I N G ----- mediation_feedback_domain.F not implemented yet for non-parallel '

#else

   IF      ( .FALSE. )                        THEN

#if (EM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM ) THEN
     CALL feedback_domain_em_part1 ( grid, nested_grid, config_flags ,  &
!
# include "em_actual_args.inc"
!
                 )
#endif
#if (NMM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_NMM ) THEN
     CALL wrf_message(' W A R N I N G ----- mediation_feedback_domain.F not implemented yet for NMM ')
#endif
#if (COAMPS_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_COAMPS ) THEN
     CALL wrf_message(' W A R N I N G ----- mediation_feedback_domain.F not implemented yet for COAMPS ')
#endif

   ELSE
     CALL wrf_message(' W A R N I N G ----- mediation_feedback_domain.F not implemented yet for this core')

   ENDIF

   grid => parent_grid
#include "deref_kludge.h"

   IF      ( .FALSE. )                        THEN

#if (EM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM ) THEN
     CALL feedback_domain_em_part2 ( grid , nested_grid%intermediate_grid, nested_grid , config_flags ,  &
!
# include "em_actual_args.inc"
!
                 )
#endif
#if (NMM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_NMM) THEN
write(0,*)' W A R N I N G ----- mediation_feedback_domain.F not implemented yet for non-parallel '
#endif
#if (COAMPS_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_COAMPS ) THEN
write(0,*)' W A R N I N G ----- mediation_feedback_domain.F not implemented yet for non-parallel '
#endif
   ENDIF

! #endif DM_PARALLEL
#endif

   RETURN
END SUBROUTINE med_feedback_domain


