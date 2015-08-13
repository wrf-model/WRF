subroutine da_sigma_v_adj(ifreq,p0,wv,hwv,ta,gamma,sigv,                   &
                           ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma,ADJ_sigma_v)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! output: ADJ_p0, ADJ_wv, ADJ_hwv, ADJ_ta, ADJ_gamma
   ! input: ADJ_sigma_v
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ifreq
   real,    intent(in)    :: p0,wv,hwv,ta,gamma  ! base field
   real,    intent(inout) :: ADJ_p0,ADJ_wv,ADJ_hwv,ADJ_ta,ADJ_gamma
   real,    intent(in)    :: ADJ_sigma_v
   real,    intent(out)   :: sigv

   real :: wvc, wvcor(4)
   real :: ADJ_wvc

   real :: voh1,otbar1,pbar1
   real :: term21,term31,term41,term51,term61
   real :: a11,a21,a31,a41,a51,a61
   real :: ADJ_voh1,ADJ_otbar1,ADJ_pbar1
   real :: ADJ_term21,ADJ_term31,ADJ_term41,ADJ_term51,ADJ_term61

   real :: voh2,otbar2,pbar2
   real :: term22,term32,term42,term52,term62
   real :: a12,a22,a32,a42,a52,a62
   real :: ADJ_voh2,ADJ_otbar2,ADJ_pbar2
   real :: ADJ_term22,ADJ_term32,ADJ_term42,ADJ_term52,ADJ_term62

   real :: voh3,otbar3,pbar3
   real :: term23,term33,term43,term53,term63
   real :: a13,a23,a33,a43,a53,a63
   real :: ADJ_voh3,ADJ_otbar3,ADJ_pbar3
   real :: ADJ_term23,ADJ_term33,ADJ_term43,ADJ_term53,ADJ_term63

   real :: voh4,otbar4,pbar4
   real :: term24,term34,term44,term54,term64
   real :: a14,a24,a34,a44,a54,a64
   real :: ADJ_voh4,ADJ_otbar4,ADJ_pbar4
   real :: ADJ_term24,ADJ_term34,ADJ_term44,ADJ_term54,ADJ_term64

   real :: const1,const2,const3,const4
   real :: h1,h2,h3,h4

   real :: ADJ_sigv

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

   if (trace_use) call da_trace_entry("da_sigma_v_adj")

   ! use modified water vapor value to correct for errors in theoretical absorption

   wvc        = 0.0
   ADJ_wvc    = 0.0
   voh1       = 0.0
   otbar1     = 0.0
   pbar1      = 0.0
   term21     = 0.0
   term31     = 0.0
   term41     = 0.0
   term51     = 0.0
   term61     = 0.0
   ADJ_voh1   = 0.0
   ADJ_otbar1 = 0.0
   ADJ_pbar1  = 0.0
   ADJ_term21 = 0.0
   ADJ_term31 = 0.0
   ADJ_term41 = 0.0
   ADJ_term51 = 0.0
   ADJ_term61 = 0.0

   voh2       = 0.0
   otbar2     = 0.0
   pbar2      = 0.0
   term22     = 0.0
   term32     = 0.0
   term42     = 0.0
   term52     = 0.0
   term62     = 0.0
   ADJ_voh2   = 0.0
   ADJ_otbar2 = 0.0
   ADJ_pbar2  = 0.0
   ADJ_term22 = 0.0
   ADJ_term32 = 0.0
   ADJ_term42 = 0.0
   ADJ_term52 = 0.0
   ADJ_term62 = 0.0

   voh3       = 0.0
   otbar3     = 0.0
   pbar3      = 0.0
   term23     = 0.0
   term33     = 0.0
   term43     = 0.0
   term53     = 0.0
   term63     = 0.0
   ADJ_voh3   = 0.0
   ADJ_otbar3 = 0.0
   ADJ_pbar3  = 0.0
   ADJ_term23 = 0.0
   ADJ_term33 = 0.0
   ADJ_term43 = 0.0
   ADJ_term53 = 0.0
   ADJ_term63 = 0.0

   voh4       = 0.0
   otbar4     = 0.0
   pbar4      = 0.0
   term24     = 0.0
   term34     = 0.0
   term44     = 0.0
   term54     = 0.0
   term64     = 0.0
   ADJ_voh4   = 0.0
   ADJ_otbar4 = 0.0
   ADJ_pbar4  = 0.0
   ADJ_term24 = 0.0
   ADJ_term34 = 0.0
   ADJ_term44 = 0.0
   ADJ_term54 = 0.0
   ADJ_term64 = 0.0

   sigv     = 0.0
   ADJ_sigv = 0.0

   wvc = wv*wvcor(ifreq)

   if (ifreq == 1) then
      pbar1  = p0/(1.0 + hwv/h1)
      voh1   = wv/hwv
      term21 = a21*voh1
      otbar1 =  1.0/(ta - const1*gamma*hwv)
      term31 = a31*otbar1
      term61 = a61*otbar1*otbar1
      term41 = a41*pbar1*otbar1
      term51 = a51*voh1*otbar1
      sigv   = a11 + term21 + term31 + term41 + term51 + term61
   else if (ifreq == 2) then
      pbar2  = p0/(1.0 + hwv/h2)
      term22 = a22*pbar2
      term52 = a52*pbar2*pbar2
      voh2   = wv/hwv
      term32 = a32*voh2
      otbar2 = 1.0/(ta - const2*gamma*hwv)
      term42 = a42*otbar2
      term62 = a62*otbar2*otbar2
      sigv   = a12 + term22 + term32 + term42 + term52 + term62
   else if (ifreq==3) then
      pbar3  = p0/(1.0 + hwv/h3)
      term43 = a43*pbar3*pbar3
      voh3   = wv/hwv
      term23 = a23*voh3
      otbar3 = 1.0/(ta - const3*gamma*hwv)
      term33 = a33*otbar3
      term53 = a53*pbar3*voh3
      term63 = a63*otbar3*voh3
      sigv   = a13 + term23 + term33 + term43 + term53 + term63
   else if (ifreq == 4) then
      pbar4  = p0/(1.0 + hwv/h4)
      term44 = a44*pbar4*pbar4
      voh4   = wv/hwv
      term24 = a24*voh4
      otbar4 = 1.0/(ta - const4*gamma*hwv)
      term34 = a34*otbar4
      term54 = a54*pbar4*voh4
      term64 = a64*otbar4*voh4
      sigv   = a14 + term24 + term34 + term44 + term54 + term64
   else
      sigv = 0.0
   end if

   ADJ_sigv = ADJ_sigma_v*wvc
   ADJ_wvc  = sigv*ADJ_sigma_v

   if (ifreq == 1) then
      ADJ_term21 = ADJ_sigv 
      ADJ_term31 = ADJ_sigv
      ADJ_term41 = ADJ_sigv
      ADJ_term51 = ADJ_sigv
      ADJ_term61 = ADJ_sigv

      ADJ_voh1   = a51*ADJ_term51*otbar1
      ADJ_otbar1 = a51*voh1*ADJ_term51

      ADJ_pbar1  = a41*ADJ_term41*otbar1
      ADJ_otbar1 = a41*pbar1*ADJ_term41 + ADJ_otbar1
      ADJ_otbar1 = 2.0*a61*otbar1*ADJ_term61 + ADJ_otbar1

      ADJ_otbar1 = a31*ADJ_term31 + ADJ_otbar1

      ADJ_ta    = -otbar1*otbar1*ADJ_otbar1  + ADJ_ta
      ADJ_hwv   = otbar1*otbar1*const1*gamma*ADJ_otbar1  + ADJ_hwv
      ADJ_gamma = otbar1*otbar1*const1*ADJ_otbar1*hwv  + ADJ_gamma      

      ADJ_voh1  = a21*ADJ_term21 + ADJ_voh1

      ADJ_wv    = ADJ_voh1/hwv  + ADJ_wv
      ADJ_hwv   = -voh1*ADJ_voh1/hwv + ADJ_hwv

      ADJ_p0    = ADJ_pbar1/(1.0 + hwv/h1)  + ADJ_p0
      ADJ_hwv   = -pbar1*ADJ_pbar1/(h1*(1.0 + hwv/h1)) + ADJ_hwv
   else if (ifreq==2) then
      ADJ_term22 = ADJ_sigv 
      ADJ_term32 = ADJ_sigv
      ADJ_term42 = ADJ_sigv
      ADJ_term52 = ADJ_sigv
      ADJ_term62 = ADJ_sigv

      ADJ_otbar2 = 2.0*a62*otbar2*ADJ_term62

      ADJ_otbar2 = a42*ADJ_term42 + ADJ_otbar2

      ADJ_ta     = -otbar2*otbar2*ADJ_otbar2  + ADJ_ta
      ADJ_hwv    =  otbar2*otbar2*const2*gamma*ADJ_otbar2 + ADJ_hwv
      ADJ_gamma  =  otbar2*otbar2*const2*ADJ_otbar2*hwv + ADJ_gamma

      ADJ_voh2   = a32*ADJ_term32

      ADJ_wv     = ADJ_voh2/hwv + ADJ_wv
      ADJ_hwv    = -voh2*ADJ_voh2/hwv + ADJ_hwv

      ADJ_pbar2  = 2.0*a52*pbar2*ADJ_term52

      ADJ_pbar2  = a22*ADJ_term22 + ADJ_pbar2

      ADJ_p0     = ADJ_pbar2/(1.0 + hwv/h2) + ADJ_p0
      ADJ_hwv    = -pbar2*ADJ_pbar2/h2/(1.0 + hwv/h2) + ADJ_hwv
   else if (ifreq==3) then
      ADJ_term23 = ADJ_sigv
      ADJ_term33 = ADJ_sigv
      ADJ_term43 = ADJ_sigv
      ADJ_term53 = ADJ_sigv
      ADJ_term63 = ADJ_sigv

      ADJ_otbar3 = a63*ADJ_term63*voh3
      ADJ_voh3   = a63*otbar3*ADJ_term63

      ADJ_pbar3  = a53*ADJ_term53*voh3
      ADJ_voh3   = a53*pbar3*ADJ_term53 + ADJ_voh3

      ADJ_otbar3 = a33*ADJ_term33 + ADJ_otbar3

      ADJ_ta     = -otbar3*otbar3*ADJ_otbar3 + ADJ_ta
      ADJ_hwv    =  otbar3*otbar3*const3*gamma*ADJ_otbar3 + ADJ_hwv
      ADJ_gamma  =  otbar3*otbar3*const3*ADJ_otbar3*hwv + ADJ_gamma

      ADJ_voh3   = a23*ADJ_term23 + ADJ_voh3

      ADJ_wv     = ADJ_voh3/hwv  + ADJ_wv
      ADJ_hwv    = -voh3*ADJ_voh3/hwv + ADJ_hwv

      ADJ_pbar3 = 2.0*a43*pbar3*ADJ_term43 + ADJ_pbar3

      ADJ_p0    = ADJ_pbar3/(1.0 + hwv/h3) + ADJ_p0
      ADJ_hwv   = -pbar3*ADJ_pbar3/h3/(1.0 + hwv/h3) + ADJ_hwv
   else if (ifreq == 4) then
      ADJ_term24 = ADJ_sigv
      ADJ_term34 = ADJ_sigv
      ADJ_term44 = ADJ_sigv
      ADJ_term54 = ADJ_sigv
      ADJ_term64 = ADJ_sigv

      ADJ_otbar4 = a64*ADJ_term64*voh4
      ADJ_voh4   = a64*otbar4*ADJ_term64 

      ADJ_pbar4  = a54*ADJ_term54*voh4
      ADJ_voh4   = a54*pbar4*ADJ_term54 + ADJ_voh4

      ADJ_otbar4 = a34*ADJ_term34 + ADJ_otbar4

      ADJ_ta     = -otbar4*otbar4*ADJ_otbar4  + ADJ_ta
      ADJ_hwv    =  otbar4*otbar4*const4*gamma*ADJ_otbar4 + ADJ_hwv
      ADJ_gamma  =  otbar4*otbar4*const4*ADJ_otbar4*hwv + ADJ_gamma

      ADJ_voh4   = a24*ADJ_term24 + ADJ_voh4

      ADJ_wv     = ADJ_voh4/hwv + ADJ_wv
      ADJ_hwv    = -voh4*ADJ_voh4/hwv + ADJ_hwv

      ADJ_pbar4  = 2.0*a44*pbar4*ADJ_term44 + ADJ_pbar4

      ADJ_p0     = ADJ_pbar4/(1.0 + hwv/h4) + ADJ_p0
      ADJ_hwv    = -pbar4*ADJ_pbar4/h4/(1.0 + hwv/h4) + ADJ_hwv
   end if

   ADJ_wv  = ADJ_wvc*wvcor(ifreq) + ADJ_wv

   if (trace_use) call da_trace_exit("da_sigma_v_adj")

end subroutine da_sigma_v_adj


