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
# include  "solve_em.int"
#endif
#if (NMM_CORE == 1)
# include  "solve_nmm.int"
#endif
#if (COAMPS_CORE == 1)
# include  "solve_coamps.int"
#endif
#if (EXP_CORE == 1)
# include  "solve_exp.int"
#endif
   END INTERFACE

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags

   INTEGER     :: idum1, idum2

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )

#if (EM_CORE == 1 && DA_CORE != 1)
   CALL solve_em  ( grid , config_flags  &
!
# include "actual_new_args.inc"
!
               )

# if ( WRF_CHEM == 1 )
   IF ( config_flags%chem_opt > 0 .or. config_flags%tracer_opt > 0 ) THEN

     CALL chem_driver  ( grid , config_flags  &
!
# include "actual_new_args.inc"
!
               )
   ENDIF
# endif
#endif
#if (NMM_CORE == 1)
   CALL solve_nmm  ( grid , config_flags   &
!
# include "actual_new_args.inc"
!
               )
# if ( WRF_CHEM == 1 )
   IF ( config_flags%chem_opt > 0  ) THEN

     CALL chem_driver  ( grid , config_flags  &
!
# include "actual_new_args.inc"
!  
               )
   ENDIF
# endif
#endif
#if (COAMPS_CORE == 1)
   CALL solve_coamps  ( grid , config_flags   &
!
# include "actual_new_args.inc"
!
               )
#endif

! ###### 4. Edit share/solve_interface.F to add call to experimental core

#if (EXP_CORE == 1)
   CALL solve_exp  ( grid              &
!
# include "exp_actual_args.inc"
!
               )
#endif

END SUBROUTINE solve_interface

