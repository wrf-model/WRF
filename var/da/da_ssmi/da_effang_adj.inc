subroutine da_effang_adj(ifreq,theta,gx2,sigma,effangv,effangh,     &
   ADJ_gx2,ADJ_sigma,ADJ_effangv,ADJ_effangh  )

   implicit none

   !-----------------------------------------------------------------
   ! Purpose: Calculate the effective zenith angle of reflected microwave
   ! radiation at SSM/I frequencies for vertical and horizontal polarization
   !
   ! Output: ADJ_gx2, ADJ_sigma
   ! Input: ADJ_effangv,ADJ_effangh,effangv,effangh
   !-----------------------------------------------------------------
 
   integer, intent(in)    :: ifreq
   real,    intent(in)    :: theta,gx2,sigma
   real,    intent(inout) :: ADJ_gx2,ADJ_sigma
   real,    intent(inout) :: ADJ_effangv,ADJ_effangh
   real,    intent(out)   :: effangv,effangh

   real c19v,c19h,c22v,c22h,c37v,c37h,c85v,c85h
   real s19v(6),s19h(6),s22v(6),s22h(6), &
        s37v(6),s37h(6),s85v(6),s85h(6)

   real :: alnsig,gg,ggg,xd,xx,xd_save,xx_save
   real :: z1,z2,z3,z4,z5,z6
   real :: y,dth,angh,angv,alnthv,alnthh,alnthv_save,alnthh_save
   real :: ADJ_alnsig,ADJ_gg,ADJ_ggg,ADJ_xd
   real :: ADJ_z1,ADJ_z2,ADJ_z3,ADJ_z4,ADJ_z5,ADJ_z6,ADJ_alnthv
   real :: ADJ_y,ADJ_dth,ADJ_angh,ADJ_angv,ADJ_xx,ADJ_alnthh

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

   if (trace_use) call da_trace_entry("da_effang_adj")

   alnsig=0.0;gg=0.0;ggg=0.0;xd=0.0;xx=0.0;xd_save=0.0;xx_save=0.0
   z1=0.0;z2=0.0;z3=0.0;z4=0.0;z5=0.0;z6=0.0;y=0.0;dth=0.0;angh=0.0
   angv=0.0;alnthv=0.0;alnthh=0.0;alnthv_save=0.0;alnthh_save=0.0
   ADJ_alnsig=0.0;ADJ_gg=0.0;ADJ_ggg=0.0;ADJ_xd=0.0
   ADJ_z1=0.0;ADJ_z2=0.0;ADJ_z3=0.0;ADJ_z4=0.0;ADJ_z5=0.0
   ADJ_z6=0.0;ADJ_alnthv=0.0;ADJ_y=0.0;ADJ_dth=0.0;ADJ_angh=0.0
   ADJ_angv=0.0;ADJ_xx=0.0;ADJ_alnthh=0.0


   if (gx2 .le. 0.0 .or. sigma .le. 0.0) then
      effangv = theta
      effangh = theta
      return
   end if
   alnsig = alog(sigma)
   gg  = gx2*gx2
   ggg = gg*gx2

   if (ifreq .eq. 1) then 
      xd =      alnsig - c19v
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2
      alnthv =  s19v(1)*z1 + s19v(2)*z2 + s19v(3)*z3 + &
                s19v(4)*z4 + s19v(5)*z5 + s19v(6)*z6
      alnthv_save = alnthv
      alnthv =  alnthv + 3.611

      xd_save = xd
      xx_save = xx

      xd =  alnsig - c19h
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2

      alnthh =  s19h(1)*z1 + s19h(2)*z2 + s19h(3)*z3 + &
                s19h(4)*z4 + s19h(5)*z5 + s19h(6)*z6
      alnthh_save = alnthh
      alnthh =  alnthh + 3.611

   else if (ifreq .eq. 2) then 
      xd =      alnsig - c22v
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2
      alnthv =  s22v(1)*z1 + s22v(2)*z2 + s22v(3)*z3 + &
                s22v(4)*z4 + s22v(5)*z5 + s22v(6)*z6

      alnthv_save = alnthv
      alnthv =  alnthv + 3.611
 
      xd_save = xd
      xx_save = xx

      xd =      alnsig - c22h
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2
      alnthh =  s22h(1)*z1 + s22h(2)*z2 + s22h(3)*z3 + &
                   s22h(4)*z4 + s22h(5)*z5 + s22h(6)*z6

      alnthh_save = alnthh
      alnthh =  alnthh + 3.611

   else if (ifreq .eq. 3) then 
      xd =      alnsig - c37v
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2
      alnthv =  s37v(1)*z1 + s37v(2)*z2 + s37v(3)*z3 + &
                s37v(4)*z4 + s37v(5)*z5 + s37v(6)*z6

      alnthv_save = alnthv
      alnthv =  alnthv + 3.611
 
      xd_save = xd
      xx_save = xx

      xd =      alnsig - c37h
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2
      alnthh =  s37h(1)*z1 + s37h(2)*z2 + s37h(3)*z3 + &
                s37h(4)*z4 + s37h(5)*z5 + s37h(6)*z6
 
      alnthh_save = alnthh
      alnthh =  alnthh + 3.611

   else if (ifreq .eq. 4) then 
      xd =      alnsig - c85v
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2
      alnthv =  s85v(1)*z1 + s85v(2)*z2 + s85v(3)*z3 + &
                s85v(4)*z4 + s85v(5)*z5 + s85v(6)*z6

      alnthv_save = alnthv
      alnthv =  alnthv + 3.611

      xd_save = xd
      xx_save = xx

      xd =      alnsig - c85h
      xx =  xd*xd
      z1 =  xx*ggg
      z2 =  xd*ggg
      z3 =  xd*gg
      z4 =  xx*gg
      z5 =  xx*gx2
      z6 =  xd*gx2
      alnthh =  s85h(1)*z1 + s85h(2)*z2 + s85h(3)*z3 + &
                s85h(4)*z4 + s85h(5)*z5 + s85h(6)*z6

      alnthh_save = alnthh
      alnthh =  alnthh + 3.611
   end if

   angv =   90.0 - exp(alnthv)
   angh =   90.0 - exp(alnthh)
       y    =   1.0 - 28.0*gx2
   if (y .lt. 0.0) then
       y = 0.0
   end if

   dth     = (theta - 53.0)*y
   effangv = angv + dth
   effangh = angh + dth

   ! start

   if (gx2 .le. 0.0 .or. sigma .le. 0.0) then
      ADJ_effangv = 0.0
      ADJ_effangh = 0.0
      return
   end if

   ADJ_angh  = ADJ_effangh
   ADJ_dth   = ADJ_effangh

   ADJ_angv  = ADJ_effangv
   ADJ_dth   = ADJ_effangv  + ADJ_dth

   ADJ_y     = (theta - 53.0)*ADJ_dth  

   if (y .lt. 0.0) then
      ADJ_y = 0.0
   end if

   ADJ_gx2  = - 28.0*ADJ_y + ADJ_gx2

   ADJ_alnthh = - ADJ_angh*exp(alnthh)
   ADJ_alnthv = - ADJ_angv*exp(alnthv)

   if (ifreq .eq. 1) then 

      alnthh = alnthh_save

      ADJ_z1  = s19h(1)*ADJ_alnthh
      ADJ_z2  = s19h(2)*ADJ_alnthh
      ADJ_z3  = s19h(3)*ADJ_alnthh
      ADJ_z4  = s19h(4)*ADJ_alnthh
      ADJ_z5  = s19h(5)*ADJ_alnthh
      ADJ_z6  = s19h(6)*ADJ_alnthh

      ADJ_xd  = ADJ_z6*gx2
      ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2
      ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg  + ADJ_xx
      ADJ_gg  = xx*ADJ_z4
      ADJ_xd  = ADJ_z3*gg  + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd
      ADJ_ggg = xd*ADJ_z2
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg

      ADJ_xd  = 2.0*xd*ADJ_xx + ADJ_xd
      ADJ_alnsig = ADJ_xd

      ! vertical
      xd =  xd_save
      xx =  xx_save

      alnthv = alnthv_save

      ADJ_z1   = s19v(1)*ADJ_alnthv  
      ADJ_z2   = s19v(2)*ADJ_alnthv 
      ADJ_z3   = s19v(3)*ADJ_alnthv 
      ADJ_z4   = s19v(4)*ADJ_alnthv
      ADJ_z5   = s19v(5)*ADJ_alnthv
      ADJ_z6   = s19v(6)*ADJ_alnthv

      ADJ_xd  = ADJ_z6*gx2 !+ ADJ_xd
      ADJ_gx2 = xd*ADJ_z6   + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2 !+ ADJ_xx
      ADJ_gx2 = xx*ADJ_z5   + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg   + ADJ_xx
      ADJ_gg  = xx*ADJ_z4  + ADJ_gg
      ADJ_xd  = ADJ_z3*gg  + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd
      ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg

      ADJ_xd  = 2.0*xd*ADJ_xx + ADJ_xd

      ADJ_alnsig = ADJ_xd + ADJ_alnsig

   else if (ifreq .eq. 2) then 

      ADJ_z1 = s22h(1)*ADJ_alnthh
      ADJ_z2 = s22h(2)*ADJ_alnthh
      ADJ_z3 = s22h(3)*ADJ_alnthh
      ADJ_z4 = s22h(4)*ADJ_alnthh
      ADJ_z5 = s22h(5)*ADJ_alnthh
      ADJ_z6 = s22h(6)*ADJ_alnthh
 
      ADJ_xd  = ADJ_z6*gx2
      ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2
      ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg  + ADJ_xx
      ADJ_gg  = xx*ADJ_z4  + ADJ_gg
      ADJ_xd  = ADJ_z3*gg  + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd
      ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx 
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg

      ADJ_xd  =  2.0*xd*ADJ_xx + ADJ_xd
      ADJ_alnsig = ADJ_xd

      ! vertical
      xd =  xd_save
      xx =  xx_save

      alnthv = alnthv_save

      ADJ_z1  = s22v(1)*ADJ_alnthv
      ADJ_z2  = s22v(2)*ADJ_alnthv
      ADJ_z3  = s22v(3)*ADJ_alnthv
      ADJ_z4  = s22v(4)*ADJ_alnthv
      ADJ_z5  = s22v(5)*ADJ_alnthv
      ADJ_z6  = s22v(6)*ADJ_alnthv

      ADJ_xd  = ADJ_z6*gx2
      ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2 
      ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg  + ADJ_xx
      ADJ_gg  = xx*ADJ_z4  + ADJ_gg
      ADJ_xd  = ADJ_z3*gg  + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd
      ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
      ADJ_xd  =  2.0*xd*ADJ_xx + ADJ_xd
      ADJ_alnsig = ADJ_xd     + ADJ_alnsig

   else if (ifreq .eq. 3) then 

      ADJ_z1  = s37h(1)*ADJ_alnthh
      ADJ_z2  = s37h(2)*ADJ_alnthh
      ADJ_z3  = s37h(3)*ADJ_alnthh
      ADJ_z4  = s37h(4)*ADJ_alnthh
      ADJ_z5  = s37h(5)*ADJ_alnthh
      ADJ_z6  = s37h(6)*ADJ_alnthh

      ADJ_xd  = ADJ_z6*gx2
      ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2 
      ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg  + ADJ_xx
      ADJ_gg  = xx*ADJ_z4  + ADJ_gg
      ADJ_xd  = ADJ_z3*gg  + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd
      ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
      ADJ_xd  = 2.0*xd*ADJ_xx + ADJ_xd
      ADJ_alnsig = ADJ_xd

      ! vertical
      xd =  xd_save
      xx =  xx_save

      alnthv = alnthv_save

      ADJ_z1  = s37v(1)*ADJ_alnthv
      ADJ_z2  = s37v(2)*ADJ_alnthv
      ADJ_z3  = s37v(3)*ADJ_alnthv
      ADJ_z4  = s37v(4)*ADJ_alnthv
      ADJ_z5  = s37v(5)*ADJ_alnthv
      ADJ_z6  = s37v(6)*ADJ_alnthv
  
      ADJ_xd  = ADJ_z6*gx2
      ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2
      ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg  + ADJ_xx
      ADJ_gg  = xx*ADJ_z4  + ADJ_gg
      ADJ_xd  =ADJ_z3*gg   + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd 
      ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
      ADJ_xd  = 2.0*xd*ADJ_xx + ADJ_xd
      ADJ_alnsig = ADJ_xd  + ADJ_alnsig

   else if (ifreq .eq. 4) then 

      ADJ_z1  = s85h(1)*ADJ_alnthh
      ADJ_z2  = s85h(2)*ADJ_alnthh
      ADJ_z3  = s85h(3)*ADJ_alnthh
      ADJ_z4  = s85h(4)*ADJ_alnthh
      ADJ_z5  = s85h(5)*ADJ_alnthh
      ADJ_z6  = s85h(6)*ADJ_alnthh

      ADJ_xd  = ADJ_z6*gx2
      ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2
      ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg  + ADJ_xx
      ADJ_gg  = xx*ADJ_z4  + ADJ_gg
      ADJ_xd  = ADJ_z3*gg  + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd
      ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
      ADJ_xd  = 2.0*xd*ADJ_xx + ADJ_xd
      ADJ_alnsig = ADJ_xd

      ! vertical
      xd =  xd_save
      xx =  xx_save

      alnthv = alnthv_save

      ADJ_z1  = s85v(1)*ADJ_alnthv
      ADJ_z2  = s85v(2)*ADJ_alnthv
      ADJ_z3  = s85v(3)*ADJ_alnthv
      ADJ_z4  = s85v(4)*ADJ_alnthv
      ADJ_z5  = s85v(5)*ADJ_alnthv
      ADJ_z6  = s85v(6)*ADJ_alnthv

      ADJ_xd  = ADJ_z6*gx2
      ADJ_gx2 = xd*ADJ_z6  + ADJ_gx2
      ADJ_xx  = ADJ_z5*gx2
      ADJ_gx2 = xx*ADJ_z5  + ADJ_gx2
      ADJ_xx  = ADJ_z4*gg  + ADJ_xx
      ADJ_gg  = xx*ADJ_z4  + ADJ_gg
      ADJ_xd  = ADJ_z3*gg  + ADJ_xd
      ADJ_gg  = xd*ADJ_z3  + ADJ_gg
      ADJ_xd  = ADJ_z2*ggg + ADJ_xd
      ADJ_ggg = xd*ADJ_z2  + ADJ_ggg
      ADJ_xx  = ADJ_z1*ggg + ADJ_xx
      ADJ_ggg = xx*ADJ_z1  + ADJ_ggg
      ADJ_xd  = 2.0*xd*ADJ_xx  + ADJ_xd
      ADJ_alnsig = ADJ_xd  + ADJ_alnsig
   end if

   ADJ_gg  = ADJ_ggg*gx2   + ADJ_gg
   ADJ_gx2 = gg*ADJ_ggg    + ADJ_gx2
   ADJ_gx2 = 2.0*gx2*ADJ_gg + ADJ_gx2

   if (abs(sigma) .gt. 0.0) then
      ADJ_sigma = ADJ_alnsig/sigma + ADJ_sigma
   ! else
   !    ADJ_sigma = 0.0
   end if

   if (trace_use) call da_trace_exit("da_effang_adj")

end subroutine da_effang_adj


