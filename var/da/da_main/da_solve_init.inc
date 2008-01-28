subroutine da_solve_init(grid &
#include "dummy_new_args.inc"
)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)      :: grid

#include "dummy_new_decl.inc"

#ifdef DM_PARALLEL
   integer :: ii
#endif

   integer :: sm31,sm32,sm33,sm31x,sm32x,sm33x,sm31y,sm32y,sm33y

   ! if (dwordsize != rwordsize)
#define true_MSG_XPOSE add_msg_xpose_real
   ! else
   !    define true_MSG_XPOSE add_msg_xpose_doubleprecision
   ! end if

   if (trace_use) call da_trace_entry("da_solve_init")

   ! De-reference dimension information stored in the grid data structure.

   call da_copy_dims(grid)

   ! Compute these starting and stopping locations for each tile and number 
   ! of tiles.

   call set_tiles (grid , ids , ide , jds , jde , ips , ipe , jps , jpe)

   call da_copy_tile_dims(grid)

   sm31             = grid%sm31
   sm32             = grid%sm32
   sm33             = grid%sm33
   sm31x            = grid%sm31x
   sm32x            = grid%sm32x
   sm33x            = grid%sm33x
   sm31y            = grid%sm31y
   sm32y            = grid%sm32y
   sm33y            = grid%sm33y

#ifdef DM_PARALLEL
#define REGISTER_I1
#include "data_calls.inc"

   if (trace_use) call da_trace("da_solve_init", &
      Message="Setup halo region communication")

   ! Define halo region communication.
   !-----------------------------------------------------------------------
   !  Stencils for patch communications
   !                           * * * * *
   !         *        * * *    * * * * *
   !       * + *      * + *    * * + * *
   !         *        * * *    * * * * *
   !                           * * * * *
   !ij vp%v1            x
   !ij xb%cori          x
   !ij xb%rho           x
   !ij xa%u             x
   !ij xa%v             x
   !--------------------------------------------------------------
#include "HALO_INIT.inc"
#include "HALO_PSICHI_UV.inc"
#include "HALO_BAL_EQN_ADJ.inc"
#include "HALO_PSICHI_UV_ADJ.inc"
#include "HALO_XA.inc"
#include "HALO_SFC_XA.inc"
#include "HALO_SSMI_XA.inc"
#include "HALO_2D_WORK.inc"
#include "HALO_RADAR_XA_W.inc"

   if (trace_use) call da_trace("da_solve_init", &
      Message="Copy domain and transpose descriptors")

   ! Copy domain and transpose descriptors.

   grid%xp%domdesc = grid%domdesc
   do ii = 1, max_comms
     grid%xp%comms(ii) = grid%comms(ii)
   end do

#endif

   ! Fill background scalars:

   grid%xb%ids = grid%xp%ids 
   grid%xb%ide = grid%xp%ide
   grid%xb%jds = grid%xp%jds 
   grid%xb%jde = grid%xp%jde
   grid%xb%kds = grid%xp%kds 
   grid%xb%kde = grid%xp%kde 

   grid%xb%ims = grid%xp%ims 
   grid%xb%ime = grid%xp%ime
   grid%xb%jms = grid%xp%jms 
   grid%xb%jme = grid%xp%jme
   grid%xb%kms = grid%xp%kms 
   grid%xb%kme = grid%xp%kme 

   grid%xb%its = grid%xp%its 
   grid%xb%ite = grid%xp%ite
   grid%xb%jts = grid%xp%jts 
   grid%xb%jte = grid%xp%jte 
   grid%xb%kts = grid%xp%kts
   grid%xb%kte = grid%xp%kte 

   if (trace_use) call da_trace_exit("da_solve_init")

end subroutine da_solve_init


