      subroutine roughem(ifreq,gx2,tk,theta,remv,remh)
!
! Calculates rough-surface emissivity of ocean surface at SSM/I
! frequencies.
!
      integer :: ifreq
      real, intent(in ) :: gx2,tk,theta
      real, intent(out) :: remv,remh
      real tp,dtheta,g,x1,x2,x3,x4,semv,semh
      real ssw

      real a19v(4),a22v(4),a37v(4),a85v(4)
      real a19h(4),a22h(4),a37h(4),a85h(4)
      real f(4)
!
      data a19v/  -0.111E+01,   0.713E+00,  -0.624E-01,   0.212E-01 /
      data a19h/   0.812E+00,  -0.215E+00,   0.255E-01,   0.305E-02 /
      data a22v/  -0.134E+01,   0.911E+00,  -0.893E-01,   0.463E-01 /
      data a22h/   0.958E+00,  -0.350E+00,   0.566E-01,  -0.262E-01 /
      data a37v/  -0.162E+01,   0.110E+01,  -0.730E-01,   0.298E-01 /
      data a37h/   0.947E+00,  -0.320E+00,   0.624E-01,  -0.300E-01 /
      data a85v/  -0.145E+01,   0.808E+00,  -0.147E-01,  -0.252E-01 /
      data a85h/   0.717E+00,  -0.702E-01,   0.617E-01,  -0.243E-01 /
!
      data f/ 19.35, 22.235, 37.0, 85.5 /
!
      tp = tk/t_roughem
      dtheta = theta-53.0
      g =  0.5*gx2
      x1 = g
      x2 = tp*g
      x3 = dtheta*g
      x4 = tp*x3
!
      if (ifreq .eq. 1) then
         remv = x1*a19v(1) + x2*a19v(2) + x3*a19v(3) + x4*a19v(4)
         remh = x1*a19h(1) + x2*a19h(2) + x3*a19h(3) + x4*a19h(4)
      else if (ifreq .eq. 2) then
         remv = x1*a22v(1) + x2*a22v(2) + x3*a22v(3) + x4*a22v(4)
         remh = x1*a22h(1) + x2*a22h(2) + x3*a22h(3) + x4*a22h(4)
      else if (ifreq .eq. 3) then
         remv = x1*a37v(1) + x2*a37v(2) + x3*a37v(3) + x4*a37v(4)
         remh = x1*a37h(1) + x2*a37h(2) + x3*a37h(3) + x4*a37h(4)
      else if (ifreq .eq. 4) then
         remv = x1*a85v(1) + x2*a85v(2) + x3*a85v(3) + x4*a85v(4)
         remh = x1*a85h(1) + x2*a85h(2) + x3*a85h(3) + x4*a85h(4)
      end if
      ssw=36.5
      call spemiss(f(ifreq),tk,theta,ssw,semv,semh)
      remv = remv + semv
      remh = remh + semh
  
      end subroutine roughem
