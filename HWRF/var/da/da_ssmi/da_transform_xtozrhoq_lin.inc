subroutine da_transform_xtozrhoq_lin(xb, xa, i, j, zh, zf, tgl_zrhom)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none
   
   integer,        intent(in)      :: i, j
   type (xb_type), intent(in)      :: xb         ! first guess state.
   type (x_type) , intent(in)      :: xa         ! increment
   real,           intent(out)     :: TGL_zrhom  ! gridded height weighted moisture
   real,           intent(in)      :: zh(mkz)
   real,           intent(in)      :: zf(mkz+1)

   integer                         :: k

   if (trace_use) call da_trace_entry("da_transform_xtozrhoq_lin")

   TGL_zrhom = 0.0

   do k = 1,mkz
      TGL_zrhom = (zf(k)-zf(k+1))*zh(k)*(xb%q(i,j,k)*xa%rho(i,j,k) + &
                                         xa%q(i,j,k)*xb%rho(i,j,k)   &
                                       ) + TGL_zrhom
   end do

   if (trace_use) call da_trace_exit("da_transform_xtozrhoq_lin")
 
end subroutine da_transform_xtozrhoq_lin


