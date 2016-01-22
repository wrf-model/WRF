subroutine da_tpq_to_slp_lin ( T, Q, P, TERR, PSFC, T9, Q9, P9, PSFC9, SLP9)

   !-----------------------------------------------------------------------
   ! Purpose: computes sea level pressure from the rule                
   !              t1/t2=(p1/p2)**(gamma*r/g).                              
   !                                                                       
   !     input       t        temperature
   !                 q        mixing ratio
   !                 p        pressure
   !                 terr     terrain
   !                 psfc     surface pressure
   !                                                                       
   !     output      slp      sea level pressure
   !-----------------------------------------------------------------------

   implicit none
              
   real, intent(in)    :: terr, psfc, psfc9
   real, intent(in)    :: t(kms:kme)
   real, intent(in)    :: q(kms:kme)
   real, intent(in)    :: p(kms:kme)
   real, intent(in)    :: t9(kms:kme)
   real, intent(in)    :: q9(kms:kme)
   real, intent(in)    :: p9(kms:kme)
   ! real                :: slp
   real, intent(out)   :: slp9

   integer :: k, klo, khi
   real    :: pl, t0, ts, xterm,tlo, thi, tl
   real    :: pl9,t09,ts9,tlo9,thi9,tl9,coef1,coef2

   real, parameter      :: gamma  = 6.5e-3
   real, parameter      :: tc     = t_kelvin+17.5
   real, parameter      :: pconst = 10000.0
   real, parameter      :: eps    = 0.622

   if (trace_use) call da_trace_entry("da_tpq_to_slp_lin")
                                                                          
   !     ... sea level pressure                                            
                                                                          
   xterm=gamma* gas_constant / gravity                                                   
                                                                          
   ! compute pressure at pconst mb above surface (pl)                                                                                     

   if (terr <= 0.0) then
      slp9 = psfc9
      ! slp = psfc
      if (trace_use) call da_trace_exit("da_tpq_to_slp_lin")
      return
   end if


   pl9  = psfc9 
   pl  = psfc - pconst                                        
   klo = 0

   ! find 2 levels on sigma surfaces surrounding pl at each i,j    

   k_loop: do k=kts, kte-1
      if ((p(k) >= pl) .and. (p(k+1) < pl)) then
         khi = k+1
         klo = k
         exit k_loop
      end if
   end do k_loop

   if (klo < 1) then                                      
      write(unit=message(1),fmt='(A,F11.3,A)') &
           'error finding pressure level ',pconst,' mb above the surface'
      write(unit=message(2),fmt='(A,F11.3,2X,A,F11.3)') 'PL=',PL,'  PSFC=',psfc
      call da_error(__FILE__,__LINE__,message(1:2))                                               
   end if                                                         

   ! get temperature at pl (tl), extrapolate t at surface (ts)     
   ! and t at sea level (t0) with 6.5 k/km lapse rate              

   tlo9=t9(klo) * (eps+q(klo))/(eps*(1.0+q(klo))) + &
        q9(klo)*t(klo)*(1.0-eps)/(eps*(1.0+q(klo))**2)
   tlo=t(klo) * (eps+q(klo))/(eps*(1.0+q(klo)))
   thi9=t9(khi) * (eps+q(khi))/(eps*(1.0+q(khi)))+   &
        q9(khi)*t(khi)*(1.0-eps)/(eps*(1.0+q(khi))**2)
   thi=t(khi) * (eps+q(khi))/(eps*(1.0+q(khi)))
   coef1=alog(pl/p(khi))
   coef2=alog(p(klo)/p(khi))
   tl9=(1.0-coef1/coef2)*thi9+coef1/coef2*tlo9       &
       -(thi-tlo)/(coef2*pl)*pl9                 &
       +((thi-tlo)/(coef2*p(khi))*(1.0-coef1/coef2))*p9(khi)   &
       +(thi-tlo)*coef1/(coef2*coef2*p(klo))*p9(klo)
   tl=thi-(thi-tlo)*coef1/coef2
   ts9=tl9*(psfc/pl)**xterm+psfc9*xterm*(tl/pl)*(psfc/pl)**  &
       (xterm-1)-pl9*xterm*(tl*psfc/(pl*pl))*(psfc/pl)**(xterm-1)
   ts=tl*(psfc/pl)**xterm                           
   t09=ts9
   t0=ts +gamma*terr

   ! correct sea level temperature if too hot                      

   if ( t0 >= tc ) then
      if ( ts <= tc ) then
         t09 = 0.0
         t0 = tc
      else
         t09 = -0.01*(ts-tc)*ts9
         t0 = tc-0.005*(ts-tc)**2
      end if
   end if

   ! compute sea level pressure                                    

   slp9=psfc9*exp(2.0*gravity*terr/(gas_constant*(ts+t0)))  &
          -psfc*exp(2.0*gravity*terr/(gas_constant*(ts+t0)))*  &
          2.0*gravity*terr/(gas_constant*(ts+t0)**2)*(ts9+t09)
   ! slp=psfc*exp(2.0*gravity*terr/(gas_constant*(ts+t0)))

   if (trace_use) call da_trace_exit("da_tpq_to_slp_lin")

end subroutine da_tpq_to_slp_lin


