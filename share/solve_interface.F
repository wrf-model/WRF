!WRF:MEDIATION_LAYER:ADT_BARRIER
!

SUBROUTINE solve_interface ( grid ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure
   USE module_wrf_error

   IMPLICIT NONE

   INTERFACE
#if (EM_CORE == 1 && DA_CORE != 1)
# include  <solve_em.int>
#endif
#if (NMM_CORE == 1)
# include  <solve_nmm.int>
#endif
#if (COAMPS_CORE == 1)
# include  <solve_coamps.int>
#endif
#if (EXP_CORE == 1)
# include  <solve_exp.int>
#endif
   END INTERFACE

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   INTEGER     :: idum1, idum2

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

#include "deref_kludge.h"

   IF ( config_flags%dyn_opt == DYN_NODYN  ) THEN

     CALL wrf_debug( 1 , "solve_interface: dynamics disabled\n" )

#if (EM_CORE == 1 && DA_CORE != 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EM  ) THEN
     CALL solve_em  ( grid , config_flags , &
!
# include <em_actual_args.inc>
!
               )

# ifdef WRF_CHEM
   IF ( config_flags%chem_opt == RADM2  ) THEN
     CALL chem_driver  ( grid , config_flags , &
!
# include <em_actual_args.inc>
!
               )
   ENDIF
   IF ( config_flags%chem_opt == RADM2SORG ) THEN
     CALL chem_driver  ( grid , config_flags , &
!
# include <em_actual_args.inc>
!
               )
   ENDIF
# endif
#endif
#if (NMM_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_NMM  ) THEN
     CALL solve_nmm  ( grid , config_flags , &
!
# include <nmm_actual_args.inc>
!
               )
#endif
#if (COAMPS_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_COAMPS  ) THEN
     CALL solve_coamps  ( grid , config_flags , &
!
# include <coamps_actual_args.inc>
!
               )
#endif

! ###### 4. Edit share/solve_interface.F to add call to experimental core

#if (EXP_CORE == 1)
   ELSE IF ( config_flags%dyn_opt == DYN_EXP  ) THEN
     CALL solve_exp  ( grid ,            &
!
# include <exp_actual_args.inc>
!
               )
#endif

   ELSE

     WRITE( wrf_err_message , * ) 'Invalid dynamics option: dyn_opt = ',config_flags%dyn_opt
     CALL wrf_error_fatal ( TRIM ( wrf_err_message ) )
     
   END IF

END SUBROUTINE solve_interface

