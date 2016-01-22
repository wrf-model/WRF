subroutine da_roughem_adj (ifreq,gx2,tk,theta,remv,remh,ADJ_gx2,ADJ_tk,ADJ_remv,ADJ_remh)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   !-----------------------------------------------------------------
   ! Input  :: ADJ_tk, ADJ_gx2
   ! Output :: ADJ_remv,ADJ_remh, remv,remh
   !
   ! Calculates rough-surface emissivity of ocean surface at SSM/I
   ! frequencies.

   integer, intent(in)    :: ifreq
   real   , intent(in)    :: tk, theta, gx2
   real   , intent(in)    :: ADJ_remv,ADJ_remh
   real   , intent(inout) :: ADJ_tk,ADJ_gx2
   real,    intent(out)   :: remv,remh

   real :: ssw

   real :: a19v(4),a22v(4),a37v(4),a85v(4)
   real :: a19h(4),a22h(4),a37h(4),a85h(4)
   real :: f(4)
   real :: semv,semh,ADJ_semv,ADJ_semh,remv_save,remh_save
   real :: tp,g,x1,x2,x3,x4,dtheta
   real :: ADJ_tp,ADJ_g,ADJ_x1,ADJ_x2,ADJ_x3,ADJ_x4

   data a19v/  -0.111E+01,   0.713E+00,  -0.624E-01,   0.212E-01 /
   data a19h/   0.812E+00,  -0.215E+00,   0.255E-01,   0.305E-02 /
   data a22v/  -0.134E+01,   0.911E+00,  -0.893E-01,   0.463E-01 /
   data a22h/   0.958E+00,  -0.350E+00,   0.566E-01,  -0.262E-01 /
   data a37v/  -0.162E+01,   0.110E+01,  -0.730E-01,   0.298E-01 /
   data a37h/   0.947E+00,  -0.320E+00,   0.624E-01,  -0.300E-01 /
   data a85v/  -0.145E+01,   0.808E+00,  -0.147E-01,  -0.252E-01 /
   data a85h/   0.717E+00,  -0.702E-01,   0.617E-01,  -0.243E-01 /

   data f/ 19.35, 22.235, 37.0, 85.5 /

   if (trace_use) call da_trace_entry("da_roughem_adj")

   semv      = 0.0
   semh      = 0.0
   ADJ_semv  = 0.0
   ADJ_semh  = 0.0
   remv_save = 0.0
   remh_save = 0.0
   tp        = 0.0
   g         = 0.0
   x1        = 0.0
   x2        = 0.0
   x3        = 0.0
   x4        = 0.0
   dtheta    = 0.0
   ADJ_tp    = 0.0
   ADJ_g     = 0.0
   ADJ_x1    = 0.0
   ADJ_x2    = 0.0
   ADJ_x3    = 0.0
   ADJ_x4    = 0.0

   tp     = tk/t_roughem
   dtheta = theta-53.0
   g      = 0.5* gx2
   x1     = g
   x2     = tp*g
   x3     = dtheta* g
   x4     = tp*x3

   if (ifreq == 1) then
      remv = x1*a19v(1) +     x2*a19v(2) +     x3*a19v(3) +     x4*a19v(4)
      remh = x1*a19h(1) +     x2*a19h(2) +     x3*a19h(3) +     x4*a19h(4)
   else if (ifreq == 2) then
      remv = x1*a22v(1) +     x2*a22v(2) +     x3*a22v(3) +     x4*a22v(4)
      remh = x1*a22h(1) +     x2*a22h(2) +     x3*a22h(3) +     x4*a22h(4)
   else if (ifreq == 3) then
      remv = x1*a37v(1) +     x2*a37v(2) +     x3*a37v(3) +     x4*a37v(4)
      remh = x1*a37h(1) +     x2*a37h(2) +     x3*a37h(3) +     x4*a37h(4)
   else if (ifreq == 4) then
      remv = x1*a85v(1) +     x2*a85v(2) +     x3*a85v(3) +     x4*a85v(4)
      remh = x1*a85h(1) +     x2*a85h(2) +     x3*a85h(3) +     x4*a85h(4)
   end if

   ssw=36.5
   call spemiss(f(ifreq),tk,theta,ssw,semv,semh)

   remv_save = remv
   remh_save = remh

   ! start

   ADJ_semh = ADJ_remh
     
   ADJ_semv = ADJ_remv

   remv = remv_save
   remh = remh_save

   call da_spemiss_adj(f(ifreq),tk,theta,ssw,semv,semh, ADJ_tk,ADJ_semv,ADJ_semh          )

 
   if (ifreq == 1) then
      ADJ_x1 = ADJ_remh*a19h(1)
      ADJ_x2 = ADJ_remh*a19h(2)
      ADJ_x3 = ADJ_remh*a19h(3)
      ADJ_x4 = ADJ_remh*a19h(4)
 
      ADJ_x1 = ADJ_remv*a19v(1) + ADJ_x1
      ADJ_x2 = ADJ_remv*a19v(2) + ADJ_x2
      ADJ_x3 = ADJ_remv*a19v(3) + ADJ_x3
      ADJ_x4 = ADJ_remv*a19v(4) + ADJ_x4
   else if (ifreq == 2) then
      ADJ_x1 = ADJ_remh*a22h(1) 
      ADJ_x2 = ADJ_remh*a22h(2)
      ADJ_x3 = ADJ_remh*a22h(3)
      ADJ_x4 = ADJ_remh*a22h(4)

      ADJ_x1 = ADJ_remv*a22v(1) + ADJ_x1
      ADJ_x2 = ADJ_remv*a22v(2) + ADJ_x2
      ADJ_x3 = ADJ_remv*a22v(3) + ADJ_x3
      ADJ_x4 = ADJ_remv*a22v(4) + ADJ_x4
   else if (ifreq == 3) then
      ADJ_x1 = ADJ_remh*a37h(1)
      ADJ_x2 = ADJ_remh*a37h(2)
      ADJ_x3 = ADJ_remh*a37h(3)
      ADJ_x4 = ADJ_remh*a37h(4)         

      ADJ_x1 = ADJ_remv*a37v(1) + ADJ_x1
      ADJ_x2 = ADJ_remv*a37v(2) + ADJ_x2
      ADJ_x3 = ADJ_remv*a37v(3) + ADJ_x3
      ADJ_x4 = ADJ_remv*a37v(4) + ADJ_x4
   else if (ifreq == 4) then
      ADJ_x1 = ADJ_remh*a85h(1) 
      ADJ_x2 = ADJ_remh*a85h(2) 
      ADJ_x3 = ADJ_remh*a85h(3)
      ADJ_x4 = ADJ_remh*a85h(4) 
 
      ADJ_x1 = ADJ_remv*a85v(1) + ADJ_x1
      ADJ_x2 = ADJ_remv*a85v(2) + ADJ_x2
      ADJ_x3 = ADJ_remv*a85v(3) + ADJ_x3
      ADJ_x4 = ADJ_remv*a85v(4) + ADJ_x4
     end if

   ADJ_tp  = ADJ_x4*x3
   ADJ_x3  = tp*ADJ_x4     + ADJ_x3
   ADJ_g   = dtheta*ADJ_x3
   ADJ_tp  = ADJ_x2*g      + ADJ_tp
   ADJ_g   = tp*ADJ_x2     + ADJ_g
   ADJ_g   = ADJ_x1        + ADJ_g
   ADJ_gx2 = 0.5*ADJ_g     + ADJ_gx2
   ADJ_tk  = ADJ_tp/t_roughem  + ADJ_tk

   if (trace_use) call da_trace_exit("da_roughem_adj")
 
end subroutine da_roughem_adj


