!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE start_domain ( grid )

   USE module_domain

   IMPLICIT NONE

   !  Input data.
   TYPE (domain)          :: grid
   !  Local data.
   INTEGER                :: dyn_opt
   INTEGER :: idum1, idum2

#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
#endif

#include "deref_kludge.h"

   CALL get_dyn_opt( dyn_opt )
  
   CALL set_scalar_indices_from_config ( head_grid%id , idum1, idum2 )

   IF ( .FALSE.                  ) THEN

#if (EM_CORE == 1)
   ELSE IF (      dyn_opt .eq. DYN_EM ) THEN
     CALL start_domain_em( grid, &
!
# include <em_actual_args.inc>
!
                         )
#endif
#if (NMM_CORE == 1)
   ELSE IF (      dyn_opt .eq. DYN_NMM ) THEN
     CALL start_domain_nmm( grid, &
!
# include <nmm_actual_args.inc>
!
                         )
#endif
#if (COAMPS_CORE == 1)
   ELSE IF (      dyn_opt .eq. DYN_COAMPS ) THEN
     CALL start_domain_coamps( grid, &
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

