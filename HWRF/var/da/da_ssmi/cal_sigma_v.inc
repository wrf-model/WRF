      subroutine cal_sigma_v(ifreq,p0,wv,hwv,ta,gamma,sigma_v)
      integer ifreq
      real sigma_v,p0,wv,hwv,ta,gamma
!
      real wvc, wvcor(4)
      real sigv
!
      real voh1,otbar1,pbar1
      real term21,term31,term41,term51,term61
      real a11,a21,a31,a41,a51,a61
!
      real voh2,otbar2,pbar2
      real term22,term32,term42,term52,term62
      real a12,a22,a32,a42,a52,a62
!
      real voh3,otbar3,pbar3
      real term23,term33,term43,term53,term63
      real a13,a23,a33,a43,a53,a63
!
      real voh4,otbar4,pbar4
      real term24,term34,term44,term54,term64
      real a14,a24,a34,a44,a54,a64
!     
      real const1,const2,const3,const4
      real h1,h2,h3,h4
!
      data const1,const2,const3,const4/0.6,2.8,0.2,0.2/
      data h1,h2,h3,h4/5.0,4.9,6.8,6.4/
!
      data a11,a21,a31,a41,a51,a61/-.13747e-2,-.43061e-4, .14618e+1,  &
        .25101e-3, .14635e-1,-.18588e+3/
      data a12,a22,a32,a42,a52,a62/ .22176e-1,-.32367e-4,-.10840e-4,  &
        -.63578e-1, .16988e-7,-.29824e+2/
      data a13,a23,a33,a43,a53,a63/-.10566e-2,-.12906e-3, .56975e+0,  &
         .10828e-8,-.17551e-7, .48601e-1/
      data a14,a24,a34,a44,a54,a64/-.60808e-2,-.70936e-3, .28721e+1,  &
         .42636e-8,-.82910e-7, .26166e+0/
!
!      data wvcor/1.01,0.95,1.06,0.92/
      data wvcor/1.02,0.98,1.02,0.88/
! use modified water vapor value to correct for errors in theoretical absorption
!
      wvc = wv*wvcor(ifreq)
!
      if (ifreq.eq.1) then
        pbar1 = p0/(1.0 + hwv/h1)
        voh1   = wv/hwv
        term21 = a21*voh1
        otbar1 = 1.0/(ta - const1*gamma*hwv)
        term31 = a31*otbar1
        term61 = a61*otbar1*otbar1
        term41 = a41*pbar1*otbar1
        term51 = a51*voh1*otbar1
        sigv = a11 + term21 + term31 + term41 + term51 + term61
      else if (ifreq.eq.2) then
        pbar2 = p0/(1.0 + hwv/h2)
        term22 = a22*pbar2
        term52 = a52*pbar2*pbar2
        voh2   = wv/hwv
        term32 = a32*voh2
        otbar2 = 1.0/(ta - const2*gamma*hwv)
        term42 = a42*otbar2
        term62 = a62*otbar2*otbar2
        sigv = a12 + term22 + term32 + term42 + term52 + term62
      else if (ifreq.eq.3) then
        pbar3 = p0/(1.0 + hwv/h3)
        term43 = a43*pbar3*pbar3
        voh3   = wv/hwv
        term23 = a23*voh3
        otbar3 = 1.0/(ta - const3*gamma*hwv)
        term33 = a33*otbar3
        term53 = a53*pbar3*voh3
        term63 = a63*otbar3*voh3
        sigv = a13 + term23 + term33 + term43 + term53 + term63
      else if (ifreq.eq.4) then
        pbar4 = p0/(1.0 + hwv/h4)
        term44 = a44*pbar4*pbar4
        voh4   = wv/hwv
        term24 = a24*voh4
        otbar4 = 1.0/(ta - const4*gamma*hwv)
        term34 = a34*otbar4
        term54 = a54*pbar4*voh4
        term64 = a64*otbar4*voh4
        sigv = a14 + term24 + term34 + term44 + term54 + term64
      else
        sigv = 0.0
      endif
      sigma_v = sigv*wvc

      end subroutine cal_sigma_v

