subroutine da_integrat_dz(grid)

   !---------------------------------------------------------------------------
   ! Non-linear PW forward operator.
   ! ===============================
   !
   ! Purpose: To calculate the IWV from the model QV and PP, TT.
   !
   ! Method:  IWV = sum {QV * RHO * dZ}
   !
   !           Unit: Qv (Kg/Kg), RHO(Kg/M^3), dZ(M)
   !                 PW (cm)
   !
   ! input     : QV, PP, TT
   !
   ! output    : PW
   !
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid

   integer :: i, j, K, ij 

   real    :: pw

   if (trace_use) call da_trace_entry("da_integrat_dz")

   ! weighted sum of vertical column 

   !$OMP PARALLEL DO &
   !$OMP PRIVATE (ij, i, j, pw)
   do ij = 1, grid%num_tiles

   do j=jts, jte
      do i=its, ite
         pw = 0.0
         do k=kts, kte
            pw = pw + (grid%xb%hf(i,j,k+1)-grid%xb%hf(i,j,k)) * grid%xb%q(i,j,k)*grid%xb%rho(i,j,k)
         end do

         grid%xb%tpw(i,j) = 0.1*pw
      end do
   end do

   end do
   !$OMP END PARALLEL DO

   if (trace_use) call da_trace_exit("da_integrat_dz")

end subroutine da_integrat_dz


