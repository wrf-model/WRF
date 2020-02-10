subroutine da_radial_velocity_adj(rv,p,u,v,w,qrn,ps,x,y,z,qrn9,rho)

   !-----------------------------------------------------------------------
   ! Purpose: adjoint of da_radial_velocity_lin
   ! History:
   !    08/2017 - bug fix for Vt (Siou-Ying Jiang, CWB, Taiwan)
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)    :: x, y, z
   real, intent(in)    :: p
   real, intent(in)    :: qrn9
   real, intent(in)    :: rho
   real, intent(in)    :: ps
   real, intent(inout) :: rv
   real, intent(inout) :: u, v, w, qrn

   real :: r, alpha, vt
   real :: qrrc
   real :: qrn_g, qrn9_g

   qrn_g = qrn *1000. ! kg/kg -> g/kg
   qrn9_g= qrn9*1000. ! kg/kg -> g/kg
   qrrc = 0.01        ! g/kg

   if (trace_use) call da_trace_entry("da_radial_velocity_adj")

   r     = sqrt(x*x+y*y+z*z)
   alpha = (ps/p)**0.4

   rv = rv/r
   u  = u + rv*x
   v  = v + rv*y
   w  = w + rv*z
   vt = -rv*z

   if (qrn9_g >  qrrc) then
      qrn_g = qrn_g + vt*0.675*alpha*qrn9_g**(-0.875)*rho**0.125
      qrn   = qrn_g * 0.001  ! g/kg -> kg/kg
   end if

   if (trace_use) call da_trace_exit("da_radial_velocity_adj")

end subroutine da_radial_velocity_adj


