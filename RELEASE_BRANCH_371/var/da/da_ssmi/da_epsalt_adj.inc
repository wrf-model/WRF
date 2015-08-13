subroutine da_epsalt_adj(f,t,ssw,ADJ_t, ADJ_epsr, ADJ_epsi)

   implicit none

   !---------------------------------------------------------------------------
   ! Purpose: TBD
   ! Output: ADJ_t      (ssw is treated as a constant now)
   ! Input: ADJ_epsr, ADJ_epsi, epsr, epsi
   !---------------------------------------------------------------------------

   real, intent(in)    :: f, t
   real, intent(inout) :: ADJ_t
   real, intent(inout) :: ssw
   real, intent(in)    :: ADJ_epsr, ADJ_epsi

   complex :: cdum1,cdum2,cdum3
   complex :: ADJ_cdum1,ADJ_cdum2,ADJ_cdum3
   real    :: ssw2,ssw3,t2,t3,es,a,esnew,tau,b,sig,taunew
   real    :: delt,delt2,beta,signew,om,d1,d2
   real    :: ADJ_t2,ADJ_t3,ADJ_es,ADJ_a,ADJ_esnew,ADJ_tau,ADJ_b,ADJ_taunew
   real    :: ADJ_delt,ADJ_delt2,ADJ_beta,ADJ_signew
   real    :: ADJ_d1,ADJ_d2

   if (trace_use) call da_trace_entry("da_epsalt_adj")

   ssw2=0.0
   ssw3=0.0 
   t2=0.0
   t3=0.0
   es=0.0
   a=0.0
   esnew=0.0
   tau=0.0
   b=0.0
   sig=0.0
   taunew=0.0
   delt=0.0
   delt2=0.0
   beta=0.0 
   signew=0.0
   om=0.0
   d1=0.0
   d2=0.0
   ADJ_t2=0.0
   ADJ_t3=0.0
   ADJ_es=0.0
   ADJ_a=0.0
   ADJ_esnew=0.0
   ADJ_tau=0.0
   ADJ_b=0.0
   ADJ_taunew=0.0
   ADJ_delt=0.0
   ADJ_delt2=0.0
   ADJ_beta=0.0
   ADJ_signew=0.0
   ADJ_d1=0.0
   ADJ_d2=0.0

   if (ssw .lt. 0.0) ssw = 32.54

   ssw2     = ssw*ssw
   ssw3     = ssw2*ssw
   t2     = t*t
   t3     = t2*t
   es     = 87.134 - 1.949e-1*t - 1.276e-2*t2 + 2.491e-4*t3
   a      = 1.0 + 1.613e-5*ssw*t - 3.656e-3*ssw + 3.21e-5*ssw2 - &
            4.232e-7*ssw3
   esnew  = es*a

   tau    = 1.768e-11 - 6.086e-13*t + 1.104e-14*t2 - 8.111e-17*t3
   b      = 1.0 + 2.282e-5*ssw*t - 7.638e-4*ssw - 7.760e-6*ssw2 + &
            1.105e-8*ssw3
   taunew = tau*b

   sig    = ssw*(0.182521 - 1.46192e-3*ssw + 2.09324e-5*ssw2 - &
            1.28205e-7*ssw3)
   delt   = 25.0 - t
   delt2  = delt*delt
   beta   =   2.033e-2 + 1.266e-4*delt      + 2.464e-6*delt2       &
            - ssw*(1.849e-5 - 2.551e-7*delt + 2.551e-8*delt2)
   signew  =   sig*exp(-beta*delt)

   om  = 2.0e9*pi*f
   cdum1  = cmplx(0.0,om*taunew)
   cdum2  = cmplx(0.0,signew/(om*8.854e-12))

   cdum3  = 4.9 + (esnew-4.9)/(1.0 + cdum1) - cdum2

   ADJ_cdum3  = ADJ_epsr + ADJ_epsi *(0.0,1.0)
   ADJ_esnew  =   REAL(ADJ_cdum3/((1.0,0.0) + cdum1))
   ADJ_cdum1  = - ADJ_cdum3*(esnew-4.9)/((1.0 + cdum1)*(1.0 + cdum1))
   ADJ_cdum2  = - ADJ_cdum3


   ADJ_signew = -aimag(ADJ_cdum2/(om*8.854e-12)) 

   ADJ_taunew = om*(-aimag(ADJ_cdum1))

   ADJ_beta   = - signew*ADJ_signew*delt
   ADJ_delt   = - signew*beta*ADJ_signew

   ADJ_delt   =   1.266e-4*ADJ_beta     + ADJ_delt
   ADJ_delt2  =   2.464e-6*ADJ_beta
   ADJ_delt   =   ssw*2.551e-7*ADJ_beta + ADJ_delt
   ADJ_delt2  = - ssw*2.551e-8*ADJ_beta + ADJ_delt2

   ADJ_delt   = 2.0*delt*ADJ_delt2 + ADJ_delt

   ADJ_t      =  - ADJ_delt + ADJ_t

   ADJ_tau    = ADJ_taunew*b 
   ADJ_b      = tau*ADJ_taunew

   ADJ_t      = 2.282e-5*ssw*ADJ_b  + ADJ_t

   ADJ_t      = - 6.086e-13*ADJ_tau + ADJ_t
   ADJ_t2     =   1.104e-14*ADJ_tau
   ADJ_t3     = - 8.111e-17*ADJ_tau

   ADJ_es     = ADJ_esnew*a
   ADJ_a      = es*ADJ_esnew
   ADJ_t      = 1.613e-5*ssw*ADJ_a  + ADJ_t
   ADJ_t      = - 1.949e-1*ADJ_es   + ADJ_t
   ADJ_t2     = - 1.276e-2*ADJ_es   + ADJ_t2
   ADJ_t3     =   2.491e-4*ADJ_es   + ADJ_t3

   ADJ_t2     = ADJ_t3*t     + ADJ_t2
   ADJ_t      = t2*ADJ_t3    + ADJ_t
   ADJ_t      = 2.0*t*ADJ_t2  + ADJ_t

   if (trace_use) call da_trace_exit("da_epsalt_adj")

end subroutine da_epsalt_adj


