subroutine da_radial_velocity_adj(rv,p,u,v,w,qrn,ps,x,y,z,qrn9)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)    :: x, y, z
   real, intent(in)    :: p
   real, intent(in)    :: qrn9
   real, intent(in)    :: ps
   real, intent(inout) :: rv
   real, intent(inout) :: u, v, w, qrn

   real :: r, alpha, vt
   real :: qrrc

   qrrc = 1.0e-3

   if (trace_use) call da_trace_entry("da_radial_velocity_adj")

   r     = sqrt(x*x+y*y+z*z)
   alpha = (ps/p)**0.4

   rv = rv/r
   u  = u + rv*x
   v  = v + rv*y
   w  = w + rv*z
   vt = -rv*z

  if (use_radar_rf .or. use_radar_rhv)then
!   if (qrn9 >  0.0) then
!      qrn = qrn + vt*0.675*alpha*qrn9**(-0.875)
!   end if
   if (qrn9 >  qrrc) then
      qrn = qrn + vt*0.675*alpha*qrn9**(-0.875)
   end if
  end if
   if (trace_use) call da_trace_exit("da_radial_velocity_adj")

end subroutine da_radial_velocity_adj


