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
   INTEGER                :: dyn_opt
   INTEGER :: idum1, idum2

#if ((EM_CORE == 1) && (DA_CORE != 1))
   INTERFACE
#    include "start_domain_em.int"
   END INTERFACE
#endif

   CALL nl_get_dyn_opt( 1, dyn_opt )
  
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   IF (      dyn_opt .eq. DYN_NODYN ) THEN

#if ((EM_CORE == 1) && (DA_CORE != 1))
   ELSE IF (      dyn_opt .eq. DYN_EM ) THEN
     CALL start_domain_em( grid, allowed_to_read  &
!
# include <em_actual_new_args.inc>
!
                         )
#endif
#if (NMM_CORE == 1)
   ELSE IF (      dyn_opt .eq. DYN_NMM ) THEN
     CALL start_domain_nmm( grid, allowed_to_read   &
!
# include <nmm_actual_args.inc>
!
                         )
#endif
#if (COAMPS_CORE == 1)
   ELSE IF (      dyn_opt .eq. DYN_COAMPS ) THEN
     CALL start_domain_coamps( grid, allowed_to_read   &
!
# include <coamps_actual_args.inc>
!
                         )
#endif

!### 4a. edit share/start_domain.F to call domain inits for core if any

#if (EXP_CORE == 1)
   ELSE IF (      dyn_opt .eq. DYN_EXP ) THEN
#endif

   ELSE

     WRITE(0,*)' start_domain: unknown or unimplemented dyn_opt = ',dyn_opt
     CALL wrf_error_fatal ( ' start_domain: unknown or unimplemented dyn_opt ' ) 
   ENDIF


END SUBROUTINE start_domain

