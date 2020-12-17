subroutine da_transform_xtozrhoq(xb, i, j, zh, zf, zrhom)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   integer,        intent(in)      :: i, j
   type (xb_type), intent(in)      :: xb         ! first guess state.
   real,           intent(out)     :: zrhom      ! gridded height weighted moisture
   real,           intent(in)      :: zh(mkz)
   real,           intent(in)      :: zf(mkz+1)

   integer                         :: k

   if (trace_use) call da_trace_entry("da_transform_xtozrhoq")
   
   zrhom = 0.0

   do k = 1,mkz
      zrhom = (zf(k)-zf(k+1))*zh(k)*(xb%q(i,j,k)*xb%rho(i,j,k))+zrhom
   end do

   if (trace_use) call da_trace_exit("da_transform_xtozrhoq")
 
end subroutine da_transform_xtozrhoq


