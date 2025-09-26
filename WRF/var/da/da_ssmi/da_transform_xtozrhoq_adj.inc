subroutine da_transform_xtozrhoq_adj(grid, i, j, zh, zf, adj_zrhom)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   type (domain), intent(inout) :: grid
   integer,       intent(in)    :: i, j
   real,          intent(in)    :: ADJ_zrhom  ! gridded height weighted moisture
   real,          intent(in)    :: zh(mkz)
   real,          intent(in)    :: zf(mkz+1)

   integer                      :: k

   if (trace_use) call da_trace_entry("da_transform_xtozrhoq_adj")

   do k = 1,mkz
      grid%xa%rho(i,j,k) = (zf(k)-zf(k+1))*zh(k)*grid%xb%q(i,j,k)*ADJ_zrhom   + grid%xa%rho(i,j,k)
      grid%xa%q(i,j,k)   = (zf(k)-zf(k+1))*zh(k)*ADJ_zrhom*grid%xb%rho(i,j,k) + grid%xa%q(i,j,k)
   end do

   if (trace_use) call da_trace_exit("da_transform_xtozrhoq_adj")
 
end subroutine da_transform_xtozrhoq_adj


