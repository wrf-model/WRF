      subroutine effang(ifreq,theta,gx2,sigma,effangv,effangh)
!
! Calculated the effective zenith angle of reflected microwave radiation
! at SSM/I frequencies for vertical and horizontal polarization
!
      real theta, gx2,sigma,effangv,effangh
      integer ifreq
      real gg,ggg,xx,xd,z1,z2,z3,z4,z5,z6,alnthv
      real alnthh,angv,angh,y,alnsig,dth
!
      real c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h
      real s19v(6),s19h(6),s22v(6),s22h(6), &
           s37v(6),s37h(6),s85v(6),s85h(6)
!
      data c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h &
        /-.5108,.5306,-.5108,.5306,-.6931,.1823,-.9163,.3000/
      data s19v /.225E+2,.698E+2,-.238E+2,-.648E+1,.402E+0,.262E+1/
      data s19h /.743E+1,.507E+2,-.206E+2,-.191E+1,.648E-1,.291E+1/
      data s22v /.249E+2,.701E+2,-.240E+2,-.714E+1,.405E+0,.256E+1/
      data s22h /.498E+1,.442E+2,-.190E+2,-.129E+1,.803E-2,.277E+1/
      data s37v /.215E+2,.573E+2,-.211E+2,-.670E+1,.443E+0,.253E+1/
      data s37h /.869E+1,.571E+2,-.257E+2,-.302E+1,.237E+0,.386E+1/
      data s85v /.116E+2,.263E+2,-.101E+2,-.358E+1,.270E+0,.175E+1/
      data s85h /.736E+1,.568E+2,-.254E+2,-.248E+1,.196E+0,.387E+1/
!
      if (gx2 .le. 0.0 .or. sigma .le. 0.0) then
        effangv = theta
        effangh = theta
        return
      end if
      alnsig = alog(sigma)
      gg = gx2*gx2
      ggg = gg*gx2
      if (ifreq .eq. 1) then 
         xd = alnsig - c19v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s19v(1)*z1 + s19v(2)*z2 +s19v(3)*z3 + &
           s19v(4)*z4 + s19v(5)*z5 + s19v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c19h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s19h(1)*z1 + s19h(2)*z2 +s19h(3)*z3 + &
           s19h(4)*z4 + s19h(5)*z5 + s19h(6)*z6
         alnthh = alnthh + 3.611
      else if (ifreq .eq. 2) then 
         xd = alnsig - c22v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s22v(1)*z1 + s22v(2)*z2 +s22v(3)*z3 + &
           s22v(4)*z4 + s22v(5)*z5 + s22v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c22h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s22h(1)*z1 + s22h(2)*z2 +s22h(3)*z3 + &
           s22h(4)*z4 + s22h(5)*z5 + s22h(6)*z6
         alnthh = alnthh + 3.611
      else if (ifreq .eq. 3) then 
         xd = alnsig - c37v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s37v(1)*z1 + s37v(2)*z2 +s37v(3)*z3 + &
           s37v(4)*z4 + s37v(5)*z5 + s37v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c37h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s37h(1)*z1 + s37h(2)*z2 +s37h(3)*z3 + &
           s37h(4)*z4 + s37h(5)*z5 + s37h(6)*z6
         alnthh = alnthh + 3.611
      else if (ifreq .eq. 4) then 
         xd = alnsig - c85v
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthv = s85v(1)*z1 + s85v(2)*z2 +s85v(3)*z3 + &
           s85v(4)*z4 + s85v(5)*z5 + s85v(6)*z6
         alnthv = alnthv + 3.611
!
         xd = alnsig - c85h
         xx = xd*xd
         z1 =  xx*ggg
         z2 =  xd*ggg
         z3 =  xd*gg
         z4 =  xx*gg
         z5 =  xx*gx2
         z6 =  xd*gx2
         alnthh = s85h(1)*z1 + s85h(2)*z2 +s85h(3)*z3 + &
           s85h(4)*z4 + s85h(5)*z5 + s85h(6)*z6
         alnthh = alnthh + 3.611
      end if
      angv = 90.0 - exp(alnthv)
      angh = 90.0 - exp(alnthh)
      y  = 1.0 - 28.0*gx2
      if (y .lt. 0.0) y = 0.0
      dth = (theta - 53.0)*y
      effangv = angv + dth
      effangh = angh + dth

      end subroutine effang
