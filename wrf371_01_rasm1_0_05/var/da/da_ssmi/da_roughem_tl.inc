subroutine da_roughem_tl(ifreq,gx2,tk,theta,remv,remh, TGL_gx2,TGL_tk,TGL_remv,TGL_remh         )

   !----------------------------------------------------------------
   ! Purpose: Calculates rough-surface emissivity of ocean surface at SSM/I
   ! frequencies.
   ! Input  : TGL_tk, TGL_gx2
   ! Output : TGL_remv,TGL_remh, remv,remh
   !----------------------------------------------------------------

   implicit none

   integer, intent(in)  :: ifreq
   real,    intent(in)  :: tk, theta, gx2, TGL_tk,TGL_gx2
   real,    intent(out) :: TGL_remv,TGL_remh, remv,remh

   real :: a19v(4),a22v(4),a37v(4),a85v(4)
   real :: a19h(4),a22h(4),a37h(4),a85h(4)
   real :: f(4)
   real :: semv,semh,TGL_semv,TGL_semh,ssw
   real :: tp,g,x1,x2,x3,x4,dtheta
   real :: TGL_tp,TGL_g,TGL_x1,TGL_x2,TGL_x3,TGL_x4

   data a19v/  -0.111E+01,   0.713E+00,  -0.624E-01,   0.212E-01 /
   data a19h/   0.812E+00,  -0.215E+00,   0.255E-01,   0.305E-02 /
   data a22v/  -0.134E+01,   0.911E+00,  -0.893E-01,   0.463E-01 /
   data a22h/   0.958E+00,  -0.350E+00,   0.566E-01,  -0.262E-01 /
   data a37v/  -0.162E+01,   0.110E+01,  -0.730E-01,   0.298E-01 /
   data a37h/   0.947E+00,  -0.320E+00,   0.624E-01,  -0.300E-01 /
   data a85v/  -0.145E+01,   0.808E+00,  -0.147E-01,  -0.252E-01 /
   data a85h/   0.717E+00,  -0.702E-01,   0.617E-01,  -0.243E-01 /

   data f/ 19.35, 22.235, 37.0, 85.5 /

   if (trace_use) call da_trace_entry("da_roughem_tl")

   tp     = tk/t_roughem
   TGL_tp = TGL_tk/t_roughem
   dtheta = theta-53.0
   g      = 0.5*    gx2
   TGL_g  = 0.5*TGL_gx2
   x1     = g
   TGL_x1 = TGL_g
   x2     = tp*g
   TGL_x2 = TGL_tp*g + tp*TGL_g
   x3     = dtheta*    g
   TGL_x3 = dtheta*TGL_g
   x4     = tp*x3
   TGL_x4 = TGL_tp*x3 + tp*TGL_x3

   if (ifreq == 1) then
      remv     =     x1*a19v(1) +     x2*a19v(2) +     x3*a19v(3) +     x4*a19v(4)
      TGL_remv = TGL_x1*a19v(1) + TGL_x2*a19v(2) + TGL_x3*a19v(3) + TGL_x4*a19v(4)
      remh     =     x1*a19h(1) +     x2*a19h(2) +     x3*a19h(3) +     x4*a19h(4)
      TGL_remh = TGL_x1*a19h(1) + TGL_x2*a19h(2) + TGL_x3*a19h(3) + TGL_x4*a19h(4)
   else if (ifreq == 2) then
      remv     =     x1*a22v(1) +     x2*a22v(2) +     x3*a22v(3) +     x4*a22v(4)
      TGL_remv = TGL_x1*a22v(1) + TGL_x2*a22v(2) + TGL_x3*a22v(3) + TGL_x4*a22v(4)
      remh     =     x1*a22h(1) +     x2*a22h(2) +     x3*a22h(3) +     x4*a22h(4)
      TGL_remh = TGL_x1*a22h(1) + TGL_x2*a22h(2) + TGL_x3*a22h(3) + TGL_x4*a22h(4)
   else if (ifreq == 3) then
      remv     =     x1*a37v(1) +     x2*a37v(2) +     x3*a37v(3) +     x4*a37v(4)
      TGL_remv = TGL_x1*a37v(1) + TGL_x2*a37v(2) + TGL_x3*a37v(3) + TGL_x4*a37v(4)
      remh     =     x1*a37h(1) +     x2*a37h(2) +     x3*a37h(3) +     x4*a37h(4)
      TGL_remh = TGL_x1*a37h(1) + TGL_x2*a37h(2) + TGL_x3*a37h(3) + TGL_x4*a37h(4)
   else if (ifreq == 4) then
      remv     =     x1*a85v(1) +     x2*a85v(2) +     x3*a85v(3) +     x4*a85v(4)
      TGL_remv = TGL_x1*a85v(1) + TGL_x2*a85v(2) + TGL_x3*a85v(3) + TGL_x4*a85v(4)
      remh     =     x1*a85h(1) +     x2*a85h(2) +     x3*a85h(3) +     x4*a85h(4)
      TGL_remh = TGL_x1*a85h(1) + TGL_x2*a85h(2) + TGL_x3*a85h(3) + TGL_x4*a85h(4)
   end if

   ssw=36.5
   call da_spemiss_tl(f(ifreq),tk,theta,ssw,semv,semh, TGL_tk,TGL_semv,TGL_semh)

   TGL_remv = TGL_remv + TGL_semv
   remv     = remv + semv
   TGL_remh = TGL_remh + TGL_semh
   remh     = remh + semh

   if (trace_use) call da_trace_exit("da_roughem_tl")
   
end subroutine da_roughem_tl


