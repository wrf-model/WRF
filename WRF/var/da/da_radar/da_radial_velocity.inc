subroutine da_radial_velocity(rv,p,u,v,w,qrn,ps,x,y,z,rho)

   !-----------------------------------------------------------------------
   ! Purpose: calculate radial velocity following Sun and Crook (1997)
   ! History:
   !    08/2017 - bug fix for Vt (Siou-Ying Jiang, CWB, Taiwan)
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: x, y, z
   real, intent(in)  :: p, u, v, w, qrn, ps
   real, intent(in)  :: rho
   real, intent(out) :: rv

   real :: r, alpha, vt
   real :: qrrc
   real :: qrn_g

   qrn_g= qrn*1000. ! kg/kg -> g/kg
   qrrc = 0.01      ! g/kg
   vt = 0.0

   if (trace_use) call da_trace_entry("da_radial_velocity")

   r=sqrt(x*x+y*y+z*z)
   alpha=(ps/p)**0.4

   if (qrn_g <= qrrc)then
      vt=0.0
   else
      vt=5.4*alpha*qrn_g**0.125*rho**0.125
   end if

   rv=u*x+v*y+(w-vt)*z
   rv=rv/r

   if (trace_use) call da_trace_exit("da_radial_velocity")

end subroutine da_radial_velocity


