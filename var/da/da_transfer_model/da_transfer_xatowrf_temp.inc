#if (WRF_CHEM == 1)

subroutine da_transfer_xatowrf_temp(grid)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRF increments for emissions
   !    Author: JJ Guerrette, CU Boulder  Date: 07/25/2016
   !
   !  The following WRF fields are modified temporarily:  
   !    grid%scaleant
   !    grid%scalebb
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)        :: grid

   integer :: i, j, k
   integer :: ic

   real    :: sdmd, s1md

   if (trace_use) call da_trace_entry("da_transfer_xatowrf_temp")

   do ic = PARAM_FIRST_SCALAR, num_scaleant
      do j=jts,jte
         do i=its,ite
            do k=1, num_ant_steps
               grid%scaleant(i,j,k,ic) = exp(grid%xbch%alpha_ant(i,j,k,ic) + grid%xach%alpha_ant(i,j,k,ic))
            end do
         end do
      end do
   end do

   do ic = PARAM_FIRST_SCALAR, num_scalebb
      do j=jts,jte
         do i=its,ite
            do k=1, num_bb_steps
               grid%scalebb(i,j,k,ic) = exp(grid%xbch%alpha_bb(i,j,k,ic) + grid%xach%alpha_bb(i,j,k,ic))
            end do
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_transfer_xatowrf_temp")

end subroutine da_transfer_xatowrf_temp

#endif

