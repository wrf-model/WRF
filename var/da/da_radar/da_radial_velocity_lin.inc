subroutine da_radial_velocity_lin(rv,p,u,v,w,qrn,ps,x,y,z,qrn9,rho)

   !-----------------------------------------------------------------------
   ! Purpose: Tangent linear of da_radial_velocity
   ! History:
   !    08/2017 - bug fix for Vt (Siou-Ying Jiang, CWB, Taiwan)
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps
   real, intent(in)  :: qrn9
   real, intent(in)  :: rho
   real, intent(out) :: rv

   real    :: r, alpha, vt
   real    :: qrrc
   real    :: qrn_g, qrn9_g

   qrn_g = qrn *1000. ! kg/kg -> g/kg
   qrn9_g= qrn9*1000. ! kg/kg -> g/kg
   qrrc = 0.01        ! g/kg
   vt = 0.0

   if (trace_use) call da_trace_entry("da_radial_velocity_lin")

   r     = sqrt(x*x+y*y+z*z)
   alpha = (ps/p)**0.4

   if (qrn9_g <= qrrc)then
      vt=0.0
   else
      vt=0.675*alpha*qrn9_g**(-0.875)*qrn_g*rho**0.125
   end if

   rv = u*x+v*y+(w-vt)*z
   rv = rv/r

   if (trace_use) call da_trace_exit("da_radial_velocity_lin")

end subroutine da_radial_velocity_lin


