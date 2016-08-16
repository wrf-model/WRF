!WRF:MEDIATION_LAYER:SOLVER
!

SUBROUTINE solve_exp ( grid                &
!
#include "exp_dummy_args.inc"
!
                 )

USE module_exp


! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   !  Subroutine interface block.

   !  Input data.

   TYPE(domain) , TARGET          :: grid

   !  Definitions of dummy arguments to solve
#include "exp_dummy_decl.inc"

   !  WRF state bcs
   TYPE (grid_config_rec_type)              :: config_flags

   ! WRF state data

   ! Local data

   INTEGER                         :: k_start , k_end
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER                         :: ij , iteration
   INTEGER                         :: im , num_3d_m , ic , num_3d_c
   INTEGER                         :: loop
   INTEGER                         :: ijds, ijde
   INTEGER                         :: idum1, idum2

! storage for tendencies and decoupled state (generated from Registry)
#include "exp_i1_decl.inc"

#ifdef DEREF_KLUDGE
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33
   INTEGER     :: sm31x, em31x, sm32x, em32x, sm33x, em33x
   INTEGER     :: sm31y, em31y, sm32y, em32y, sm33y, em33y
#endif
#include "deref_kludge.h"

#define COPY_IN
#include "exp_scalar_derefs.inc"
#ifdef DM_PARALLEL
#    define REGISTER_I1
#      include "exp_data_calls.inc"
#endif

   CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )
! this sets up the P_* indices into the moisture and chem arrays
   CALL set_scalar_indices_from_config ( grid%id , idum1 , idum2 )
 
   !  De-reference dimension information stored in the grid data structure.

!                    ikj model   kij model

   ids             = grid%sd31 ! grid%sd32 
   ide             = grid%ed31 ! grid%ed32 
   jds             = grid%sd33 ! grid%sd33 
   jde             = grid%ed33 ! grid%ed33 
   kds             = grid%sd32 ! grid%sd31 
   kde             = grid%ed32 ! grid%ed31

   ims             = grid%sm31 ! grid%sm32
   ime             = grid%em31 ! grid%em32 
   jms             = grid%sm33 ! grid%sm33 
   jme             = grid%em33 ! grid%em33 
   kms             = grid%sm32 ! grid%sm31 
   kme             = grid%em32 ! grid%em31

   ips             = grid%sp31 ! grid%sp32
   ipe             = grid%ep31 ! grid%ep32
   jps             = grid%sp33 ! grid%sp33
   jpe             = grid%ep33 ! grid%ep33
   kps             = grid%sp32 ! grid%sp31
   kpe             = grid%ep32 ! grid%ep31

   k_start         = grid%sd32 ! grid%sd31
   k_end           = grid%ed32 ! grid%ed31

   ijds = min(ids, jds)
   ijde = max(ide, jde)

   !  Compute these starting and stopping locations for each tile and number of tiles.

   CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

! end of "magic"; start of experimental solver; just a goofy relaxation 

! Halo exchange on x_1 for relaxation operator in model layer subroutine
! relax_1_into_2

#ifdef DM_PARALLEL
# include "HALO_EXP_A.inc"
#endif

! Simple 4 pt average of x_1 into x_2

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles
        CALL wrf_debug ( 200 , ' call relax_1_into_2' )
        CALL relax_1_into_2 ( x_1, x_2,                            &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )
     END DO
     !$OMP END PARALLEL DO

! Update x_1 for next go 'round

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij )
     DO ij = 1 , grid%num_tiles
        CALL wrf_debug ( 200 , ' call copy_2_into_1' )
        CALL copy_2_into_1  ( x_2, x_1,                            &
                              ids, ide, jds, jde, kds, kde,        &
                              ims, ime, jms, jme, kms, kme,        &
                              grid%i_start(ij), grid%i_end(ij),    &
                              grid%j_start(ij), grid%j_end(ij),    &
                              k_start, k_end                       )
     END DO
     !$OMP END PARALLEL DO

#define COPY_OUT
#include "exp_scalar_derefs.inc"

   RETURN

END SUBROUTINE solve_exp

