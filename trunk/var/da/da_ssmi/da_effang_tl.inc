subroutine da_effang_tl(ifreq,theta,gx2,sigma,effangv,effangh,     &
   TGL_gx2,TGL_sigma,TGL_effangv,TGL_effangh )
 
   !--------------------------------------------------------------------------
   ! Purpose : Calculate the effective zenith angle of reflected microwave 
   ! radiation at SSM/I frequencies for vertical and horizontal polarization
   !
   ! Input  :: TGL_gx2, TGL_sigma
   ! Output :: TGL_effangv,TGL_effangh,effangv,effangh
   !--------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: ifreq
   real,    intent(in)  :: theta,gx2,sigma,TGL_gx2, TGL_sigma
   real,    intent(out) :: TGL_effangv,TGL_effangh,effangv,effangh

   real c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h
   real s19v(6),s19h(6),s22v(6),s22h(6), &
        s37v(6),s37h(6),s85v(6),s85h(6)

   real :: alnsig,gg,ggg,xd,xx
   real :: z1,z2,z3,z4,z5,z6,alnthv
   real :: y,dth,angh,angv,alnthh
   real :: TGL_alnsig,TGL_gg,TGL_ggg,TGL_xd
   real :: TGL_z1,TGL_z2,TGL_z3,TGL_z4,TGL_z5,TGL_z6,TGL_alnthv
   real :: TGL_y,TGL_dth,TGL_angh,TGL_angv,TGL_xx,TGL_alnthh

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

   if (trace_use) call da_trace_entry("da_effang_tl")

   if (gx2 .le. 0.0 .or. sigma .le. 0.0) then
         effangv = theta
     TGL_effangv = 0.0
         effangh = theta
     TGL_effangh = 0.0
     return
   end if
   alnsig = alog(sigma)
   if (abs(sigma) .gt. 0.0) then
      TGL_alnsig = TGL_sigma/sigma
   else
      TGL_alnsig = 0.0
   end if
       gg  = gx2*gx2
   TGL_gg  = 2.0*gx2*TGL_gx2
       ggg = gg*gx2
   TGL_ggg = TGL_gg*gx2 + gg*TGL_gx2

   if (ifreq .eq. 1) then 

          xd =      alnsig - c19v
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg + xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg + xx*TGL_gg
          z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
      alnthv =  s19v(1)*z1 + s19v(2)*z2 + s19v(3)*z3 + &
                s19v(4)*z4 + s19v(5)*z5 + s19v(6)*z6
      TGL_alnthv = s19v(1)*TGL_z1 + s19v(2)*TGL_z2 + s19v(3)*TGL_z3 + &
                   s19v(4)*TGL_z4 + s19v(5)*TGL_z5 + s19v(6)*TGL_z6
      alnthv     =     alnthv + 3.611

          xd =      alnsig - c19h
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg + xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg + xx*TGL_gg
          z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
      
      alnthh =  s19h(1)*z1 + s19h(2)*z2 + s19h(3)*z3 + &
                s19h(4)*z4 + s19h(5)*z5 + s19h(6)*z6
      TGL_alnthh = s19h(1)*TGL_z1 + s19h(2)*TGL_z2 + s19h(3)*TGL_z3 + &
                   s19h(4)*TGL_z4 + s19h(5)*TGL_z5 + s19h(6)*TGL_z6

      alnthh     =     alnthh + 3.611

   else if (ifreq .eq. 2) then 
          xd =      alnsig - c22v
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg 
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg + xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg + xx*TGL_gg
          z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
      alnthv =  s22v(1)*z1 + s22v(2)*z2 + s22v(3)*z3 + &
                s22v(4)*z4 + s22v(5)*z5 + s22v(6)*z6
      TGL_alnthv = s22v(1)*TGL_z1 + s22v(2)*TGL_z2 + s22v(3)*TGL_z3 + &
                   s22v(4)*TGL_z4 + s22v(5)*TGL_z5 + s22v(6)*TGL_z6
      alnthv     =     alnthv + 3.611
      ! TGL_alnthv = TGL_alnthv

          xd =      alnsig - c22h
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg + xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg + xx*TGL_gg
          z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
      alnthh =  s22h(1)*z1 + s22h(2)*z2 + s22h(3)*z3 + &
                s22h(4)*z4 + s22h(5)*z5 + s22h(6)*z6
      TGL_alnthh = s22h(1)*TGL_z1 + s22h(2)*TGL_z2 + s22h(3)*TGL_z3 + &
                   s22h(4)*TGL_z4 + s22h(5)*TGL_z5 + s22h(6)*TGL_z6
      alnthh     =     alnthh + 3.611
      ! TGL_alnthh = TGL_alnthh
   else if (ifreq .eq. 3) then 
          xd =      alnsig - c37v
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg  + xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg  + xx*TGL_gg
       z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
      alnthv =  s37v(1)*z1 + s37v(2)*z2 + s37v(3)*z3 + &
                s37v(4)*z4 + s37v(5)*z5 + s37v(6)*z6
      TGL_alnthv = s37v(1)*TGL_z1 + s37v(2)*TGL_z2 + s37v(3)*TGL_z3 + &
                   s37v(4)*TGL_z4 + s37v(5)*TGL_z5 + s37v(6)*TGL_z6
      alnthv     =     alnthv + 3.611
      ! TGL_alnthv = TGL_alnthv 

          xd =      alnsig - c37h
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg +  xx*TGL_ggg
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg +  xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg  +  xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg  +  xx*TGL_gg
          z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 +  xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 +  xd*TGL_gx2
      alnthh =  s37h(1)*z1 + s37h(2)*z2 + s37h(3)*z3 + &
                s37h(4)*z4 + s37h(5)*z5 + s37h(6)*z6
      TGL_alnthh = s37h(1)*TGL_z1 + s37h(2)*TGL_z2 + s37h(3)*TGL_z3 + &
                   s37h(4)*TGL_z4 + s37h(5)*TGL_z5 + s37h(6)*TGL_z6
      alnthh     =     alnthh + 3.611
      ! TGL_alnthh = TGL_alnthh
   else if (ifreq .eq. 4) then 
          xd =      alnsig - c85v
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd 
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg  + xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg  + xx*TGL_gg
          z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
      alnthv =  s85v(1)*z1 + s85v(2)*z2 + s85v(3)*z3 + &
                   s85v(4)*z4 + s85v(5)*z5 + s85v(6)*z6
      TGL_alnthv = s85v(1)*TGL_z1 + s85v(2)*TGL_z2 + s85v(3)*TGL_z3 + &
                   s85v(4)*TGL_z4 + s85v(5)*TGL_z5 + s85v(6)*TGL_z6
      alnthv     =     alnthv + 3.611
      ! TGL_alnthv = TGL_alnthv 

          xd =      alnsig - c85h
      TGL_xd =  TGL_alnsig
          xx =  xd*xd
      TGL_xx =  2.0*xd*TGL_xd
          z1 =  xx*ggg
      TGL_z1 =  TGL_xx*ggg + xx*TGL_ggg
          z2 =  xd*ggg
      TGL_z2 =  TGL_xd*ggg + xd*TGL_ggg
          z3 =  xd*gg
      TGL_z3 =  TGL_xd*gg  + xd*TGL_gg
          z4 =  xx*gg
      TGL_z4 =  TGL_xx*gg  + xx*TGL_gg
          z5 =  xx*gx2
      TGL_z5 =  TGL_xx*gx2 + xx*TGL_gx2
          z6 =  xd*gx2
      TGL_z6 =  TGL_xd*gx2 + xd*TGL_gx2
      alnthh =  s85h(1)*z1 + s85h(2)*z2 + s85h(3)*z3 + &
                s85h(4)*z4 + s85h(5)*z5 + s85h(6)*z6
      TGL_alnthh = s85h(1)*TGL_z1 + s85h(2)*TGL_z2 + s85h(3)*TGL_z3 + &
                   s85h(4)*TGL_z4 + s85h(5)*TGL_z5 + s85h(6)*TGL_z6
      alnthh     =     alnthh + 3.611
      ! TGL_alnthh = TGL_alnthh
   end if
       angv =   90.0 - exp(alnthv)
   TGL_angv = - TGL_alnthv*exp(alnthv)
       angh =   90.0 - exp(alnthh)
   TGL_angh = - TGL_alnthh*exp(alnthh)
       y    =   1.0 - 28.0*gx2
   TGL_y    = - 28.0*TGL_gx2
   if (y .lt. 0.0) then
          y = 0.0
      TGL_y = 0.0
   end if
       dth     = (theta - 53.0)*y
   TGL_dth     = (theta - 53.0)*TGL_y
       effangv =     angv +     dth
   TGL_effangv = TGL_angv + TGL_dth
       effangh =     angh +     dth
   TGL_effangh = TGL_angh + TGL_dth

   if (trace_use) call da_trace_exit("da_effang_tl")

end subroutine da_effang_tl


