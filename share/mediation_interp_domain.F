!#define ONEWAY

SUBROUTINE med_interp_domain ( parent_grid , nested_grid )
   USE module_domain
   USE module_configure
   USE module_timing
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
# include "em_dummy_args.inc"
!
                 )
         USE module_domain
         USE module_configure
         TYPE(domain), POINTER :: grid          ! name of the grid being dereferenced (must be "grid")
         TYPE(domain), POINTER :: intermediate_grid
         TYPE (grid_config_rec_type)            :: config_flags
# include <em_dummy_decl.inc>
      END SUBROUTINE interp_domain_em_part1

      SUBROUTINE interp_domain_em_part2 ( grid, nested_grid, config_flags ,  &
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
      END SUBROUTINE interp_domain_em_part2
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

! Serial code doesn't bother with intermediate grid. Go direct from Coarse to Nest.
! Only part 1 is called.

   IF      ( .FALSE. )                        THEN

#if (EM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM ) THEN
     CALL interp_domain_em_part1 ( grid , nested_grid, config_flags ,  &
!
# include "em_actual_args.inc"
!
                 )
#endif

#if (NMM_CORE == 1)
#endif
#if (COAMPS_CORE == 1)
#endif

   ENDIF

#else

   IF      ( .FALSE. )                        THEN
#if (EM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM ) THEN
#include "deref_kludge.h"
     CALL interp_domain_em_part1 ( grid , nested_grid%intermediate_grid, config_flags ,  &
!
# include "em_actual_args.inc"
!
                 )
#endif
   ENDIF

   grid => nested_grid%intermediate_grid
#include "deref_kludge.h"
   IF      ( .FALSE. )                        THEN
#if (EM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM ) THEN

     CALL interp_domain_em_part2 ( grid, nested_grid, config_flags ,  &
!
# include "em_actual_args.inc"
!
                 )
#endif

#if (NMM_CORE == 1)
#endif
#if (COAMPS_CORE == 1)
#endif

   ENDIF

! DM_PARALLEL
#endif

!#ifndef ONEWAY
!write(0,*)'calling med_history_out for nested_grid'
!write(0,*)'Timing calling med_history_out from med_interp_domain ',nested_grid%id
!CALL med_history_out ( nested_grid , config_flags, 0, 0, 0 )
!write(0,*)'back from med_history_out for nested_grid'
!#endif


   RETURN
END SUBROUTINE med_interp_domain


