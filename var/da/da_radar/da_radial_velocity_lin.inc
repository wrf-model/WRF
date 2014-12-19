subroutine da_radial_velocity_lin(rv,p,u,v,w,qrn,ps,x,y,z,qrn9)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps
   real, intent(in)  :: qrn9
   real, intent(out) :: rv

   real    :: r, alpha, vt
   real    :: qrrc

   qrrc = 1.0e-3
   vt = 0.0

   if (trace_use) call da_trace_entry("da_radial_velocity_lin")

   r     = sqrt(x*x+y*y+z*z)
   alpha = (ps/p)**0.4


   if (use_radar_rf .or. use_radar_rhv)then
      if (qrn9 <= qrrc)then
         vt=0.0
      else
         vt=0.675*alpha*qrn9**(-0.875)*qrn
      end if
   end if

!   if (qrn9 <= 0.0) then
!      vt=0.0
!   end if

!   if (qrn9 >  0.0) then
!      vt=0.675*alpha*qrn9**(-0.875)*qrn
!   end if

   rv = u*x+v*y+(w-vt)*z
   rv = rv/r

   if (trace_use) call da_trace_exit("da_radial_velocity_lin")

end subroutine da_radial_velocity_lin


