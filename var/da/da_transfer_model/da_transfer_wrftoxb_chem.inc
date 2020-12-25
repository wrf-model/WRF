subroutine da_transfer_wrftoxb_chem(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Transfers fields from WRF to first guess structure.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !
   ! Purpose: Transfers Chem fields from WRF to first guess structure.
   !    Author: Wei Sun,  MMM/NCAR,  Date: 02/11/2019
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)        :: grid

   integer :: ic, i, j, k, ij

   if (trace_use) call da_trace_entry("da_transfer_wrftoxb_chem")

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ic, ij, i, j, k )
   do ij = 1 , grid%num_tiles

   ! Need to ensure scaling factors never drop to zero, which could happen if 4D-Var increment is too large
   do ic = PARAM_FIRST_SCALAR, num_chem
      do j=grid%j_start(ij),grid%j_end(ij)
         do i=its,ite
            do k=kts,kte
                  grid%xbchem%chem_ic(i,j,k,ic) = grid%chem(i,j,k,ic)
            end do
         end do
      end do
   end do

   end do
   !$OMP END PARALLEL DO

#ifdef DM_PARALLEL
#include "HALO_CHEM_XB.inc"
#endif

   if (trace_use) call da_trace_exit("da_transfer_wrftoxb_chem")

end subroutine da_transfer_wrftoxb_chem

