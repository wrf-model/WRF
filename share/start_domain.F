!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE start_domain ( grid , allowed_to_read )

   USE module_domain
   USE module_configure

   IMPLICIT NONE

   !  Input Arguments.
   TYPE (domain)          :: grid
   LOGICAL, INTENT(IN)    :: allowed_to_read
   !  Local data.
   INTEGER :: idum1, idum2

#if ((EM_CORE == 1) && (DA_CORE != 1))
   INTERFACE
#    include "start_domain_em.int"
   END INTERFACE
#endif
#if (NMM_CORE == 1)
   INTERFACE
#    include "start_domain_nmm.int"
   END INTERFACE
#endif

   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

#if ((EM_CORE == 1) && (DA_CORE != 1))
   CALL start_domain_em( grid, allowed_to_read  &
!
# include <actual_new_args.inc>
!
                         )
#endif
#if (NMM_CORE == 1) 
   CALL start_domain_nmm( grid, allowed_to_read   &
!
# include <actual_args.inc>
!
                         )
#endif
#if (COAMPS_CORE == 1)
   CALL start_domain_coamps( grid, allowed_to_read   &
!
# include <actual_args.inc>
!
                         )
#endif


END SUBROUTINE start_domain

