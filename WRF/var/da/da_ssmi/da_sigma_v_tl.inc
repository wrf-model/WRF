subroutine da_sigma_v_tl(ifreq,p0,wv,hwv,ta,gamma,sigma_v,                &
                           TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma,TGL_sigma_v)

   !---------------------------------------------------------------------------
   ! Purpose : TBD
   ! Input             : TGL_p0, TGL_wv, TGL_hwv, TGL_ta, TGL_gamma
   ! Output            : TGL_sigma_v
   ! Output base field : sigma_v
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: ifreq
   real, intent(in  ):: p0,wv,hwv,ta,gamma  ! base field
   real, intent(in  ):: TGL_p0,TGL_wv,TGL_hwv,TGL_ta,TGL_gamma
   real, intent(out ):: TGL_sigma_v,sigma_v

   real wvc, wvcor(4)
   real TGL_wvc

   real voh1,otbar1,pbar1
   real term21,term31,term41,term51,term61
   real a11,a21,a31,a41,a51,a61
   real TGL_voh1,TGL_otbar1,TGL_pbar1
   real TGL_term21,TGL_term31,TGL_term41,TGL_term51,TGL_term61

   real voh2,otbar2,pbar2
   real term22,term32,term42,term52,term62
   real a12,a22,a32,a42,a52,a62
   real TGL_voh2,TGL_otbar2,TGL_pbar2
   real TGL_term22,TGL_term32,TGL_term42,TGL_term52,TGL_term62

   real voh3,otbar3,pbar3
   real term23,term33,term43,term53,term63
   real a13,a23,a33,a43,a53,a63
   real TGL_voh3,TGL_otbar3,TGL_pbar3
   real TGL_term23,TGL_term33,TGL_term43,TGL_term53,TGL_term63

   real voh4,otbar4,pbar4
   real term24,term34,term44,term54,term64
   real a14,a24,a34,a44,a54,a64
   real TGL_voh4,TGL_otbar4,TGL_pbar4
   real TGL_term24,TGL_term34,TGL_term44,TGL_term54,TGL_term64

   real const1,const2,const3,const4
   real h1,h2,h3,h4

   real sigv, TGL_sigv

   data const1,const2,const3,const4/0.6,2.8,0.2,0.2/
   data h1,h2,h3,h4/5.0,4.9,6.8,6.4/

   data a11,a21,a31,a41,a51,a61/-.13747e-2,-.43061e-4, .14618e+1,  &
     .25101e-3, .14635e-1,-.18588e+3/
   data a12,a22,a32,a42,a52,a62/ .22176e-1,-.32367e-4,-.10840e-4,  &
     -.63578e-1, .16988e-7,-.29824e+2/
   data a13,a23,a33,a43,a53,a63/-.10566e-2,-.12906e-3, .56975e+0,  &
      .10828e-8,-.17551e-7, .48601e-1/
   data a14,a24,a34,a44,a54,a64/-.60808e-2,-.70936e-3, .28721e+1,  &
      .42636e-8,-.82910e-7, .26166e+0/

   ! data wvcor/1.01,0.95,1.06,0.92/
   data wvcor/1.02,0.98,1.02,0.88/

   if (trace_use) call da_trace_entry("da_sigma_v_tl")

   ! use modified water vapor value to correct for errors in theoretical absorption

   wvc     = wv*wvcor(ifreq)
   TGL_wvc = TGL_wv*wvcor(ifreq)

   if (ifreq==1) then
      pbar1 = p0/(1.0 + hwv/h1)
      TGL_pbar1  = TGL_p0/(1.0 + hwv/h1)-pbar1*TGL_hwv/(h1*(1.0 + hwv/h1))
      voh1       = wv/hwv
      TGL_voh1   = TGL_wv/hwv-voh1*TGL_hwv/hwv
      term21     = a21*voh1
      TGL_term21 = a21*TGL_voh1
      otbar1     =  1.0/(ta - const1*gamma*hwv)
      TGL_otbar1 = -otbar1*otbar1*(TGL_ta-const1*gamma*TGL_hwv - const1*TGL_gamma*hwv)
      term31     = a31*otbar1
      TGL_term31 = a31*TGL_otbar1
      term61     = a61*otbar1*otbar1
      TGL_term61 = 2.0*a61*otbar1*TGL_otbar1
      term41     = a41*pbar1*otbar1
      TGL_term41 = a41*(TGL_pbar1*otbar1+pbar1*TGL_otbar1)
      term51     = a51*voh1*otbar1
      TGL_term51 = a51*(TGL_voh1*otbar1+voh1*TGL_otbar1)
      sigv       = a11 + term21 + term31 + term41 + term51 + term61
      TGL_sigv   = TGL_term21+TGL_term31+TGL_term41+TGL_term51+TGL_term61

   else if (ifreq==2) then
      pbar2      = p0/(1.0 + hwv/h2)
      TGL_pbar2  = TGL_p0/(1.0 + hwv/h2)-pbar2*TGL_hwv/h2/(1.0 + hwv/h2)
      term22     = a22*pbar2
      TGL_term22 = a22*TGL_pbar2
      term52     = a52*pbar2*pbar2
      TGL_term52 = 2.0*a52*pbar2*TGL_pbar2
      voh2       = wv/hwv
      TGL_voh2   = TGL_wv/hwv-voh2*TGL_hwv/hwv
      term32     = a32*voh2
      TGL_term32 = a32*TGL_voh2
      otbar2     = 1.0/(ta - const2*gamma*hwv)
      TGL_otbar2 = -otbar2*otbar2*(TGL_ta-const2*gamma*TGL_hwv &
                                           -const2*TGL_gamma*hwv)
      term42     = a42*otbar2
      TGL_term42 = a42*TGL_otbar2
      term62     = a62*otbar2*otbar2
      TGL_term62 = 2.0*a62*otbar2*TGL_otbar2
      sigv       = a12 + term22 + term32 + term42 + term52 + term62
      TGL_sigv   = TGL_term22 + TGL_term32 + TGL_term42 + TGL_term52 + TGL_term62

   else if (ifreq == 3) then
      pbar3      = p0/(1.0 + hwv/h3)
      TGL_pbar3  = TGL_p0/(1.0 + hwv/h3)-pbar3*TGL_hwv/h3/(1.0 + hwv/h3)
      term43     = a43*pbar3*pbar3
      TGL_term43 = 2.0*a43*pbar3*TGL_pbar3
      voh3       = wv/hwv
      TGL_voh3   = TGL_wv/hwv-voh3*TGL_hwv/hwv
      term23     = a23*voh3
      TGL_term23 = a23*TGL_voh3
      otbar3     = 1.0/(ta - const3*gamma*hwv)
      TGL_otbar3 = -otbar3*otbar3*(TGL_ta-const3*gamma*TGL_hwv &
                                        -const3*TGL_gamma*hwv)
      term33     = a33*otbar3
      TGL_term33 = a33*TGL_otbar3
      term53     = a53*pbar3*voh3
      TGL_term53 = a53*(TGL_pbar3*voh3+pbar3*TGL_voh3)
      term63     = a63*otbar3*voh3
      TGL_term63 = a63*(TGL_otbar3*voh3+otbar3*TGL_voh3)
      sigv       = a13 + term23 + term33 + term43 + term53 + term63
      TGL_sigv   = TGL_term23 + TGL_term33 + TGL_term43 + TGL_term53 + TGL_term63
   else if (ifreq == 4) then
      pbar4      = p0/(1.0 + hwv/h4)
      TGL_pbar4  = TGL_p0/(1.0 + hwv/h4)-pbar4*TGL_hwv/h4/(1.0 + hwv/h4)
      term44     = a44*pbar4*pbar4
      TGL_term44 = 2.0*a44*pbar4*TGL_pbar4
      voh4       = wv/hwv
      TGL_voh4   = TGL_wv/hwv-voh4*TGL_hwv/hwv
      term24     = a24*voh4
      TGL_term24 = a24*TGL_voh4
      otbar4     = 1.0/(ta - const4*gamma*hwv)
      TGL_otbar4 = -otbar4*otbar4*(TGL_ta-const4*gamma*TGL_hwv -const4*TGL_gamma*hwv)
      term34     = a34*otbar4
      TGL_term34 = a34*TGL_otbar4
      term54     = a54*pbar4*voh4
      TGL_term54 = a54*(TGL_pbar4*voh4+pbar4*TGL_voh4)
      term64     = a64*otbar4*voh4
      TGL_term64 = a64*(TGL_otbar4*voh4+otbar4*TGL_voh4)
      sigv       = a14 + term24 + term34 + term44 + term54 + term64
      TGL_sigv   = TGL_term24 + TGL_term34 + TGL_term44 + TGL_term54 + TGL_term64
   else
      sigv     = 0.0
      TGL_sigv = 0.0
   end if

   sigma_v     = sigv*wvc
   TGL_sigma_v = TGL_sigv*wvc+sigv*TGL_wvc

   if (trace_use) call da_trace_exit("da_sigma_v_tl")

end subroutine da_sigma_v_tl


