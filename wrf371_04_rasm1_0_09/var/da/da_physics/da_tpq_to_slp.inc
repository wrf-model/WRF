subroutine da_tpq_to_slp ( t, q, p, terr, psfc, slp)

   !-----------------------------------------------------------------------
   ! purpose:  computes sea level pressure from the rule                
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

   real, intent(in)    :: terr, psfc
   real, intent(in)    :: t(kms:kme)
   real, intent(in)    :: q(kms:kme)
   real, intent(in)    :: p(kms:kme)
   real, intent(inout) :: slp

   integer         :: k, klo, khi
   real            :: pl, t0, ts, xterm,tlo, thi, tl
                                          
   real, parameter :: gamma  = 6.5e-3
   real, parameter :: tc     = t_kelvin+17.5
   real, parameter :: pconst = 10000.0
   real, parameter :: eps    = 0.622

   if (trace_use) call da_trace_entry("da_tpq_to_slp")
                                                                       
   ! sea level pressure                                            
                                                                         
   xterm=gamma* gas_constant / gravity                                                   
                                                                       
   ! compute pressure at pconst mb above surface (pl)              
                                                                        
   if (terr <= 0.0) then
      slp = psfc
      if (trace_use) call da_trace_exit("da_tpq_to_slp")
      return
   end if

   pl  = psfc - pconst                                        
   klo = 0

   ! find 2 levels on sigma surfaces surrounding pl at each i,j    

   do k=kts, kte-1
      if ((p(k) >= pl) .and. (p(k+1) < pl)) then
         khi = k+1
         klo = k
         exit
      end if
   end do

   if (klo < 1) then                                      
      write(unit=message(1),fmt='(a,f11.3,a)') &
         'error finding pressure level ',pconst,' mb above the surface'
      write(unit=message(2),fmt='(a,f11.3,2x,a,f11.3)') 'pl=',pl,'  psfc=',psfc
      call da_error(__FILE__,__LINE__,message(1:2))                                
   end if                                                         

   ! get temperature at pl (tl), extrapolate t at surface (ts)     
   ! and t at sea level (t0) with 6.5 k/km lapse rate              

   tlo=t(klo) * (eps+q(klo))/(eps*(1.0+q(klo)))
   thi=t(khi) * (eps+q(khi))/(eps*(1.0+q(khi)))
   tl=thi-(thi-tlo)*log(pl/p(khi)) &
                      /log(p(klo)/p(khi))               
   ts=tl*(psfc/pl)**xterm                           
   t0=ts +gamma*terr

   ! correct sea level temperature if too hot                      

   if ( t0 >= tc ) then
      if ( ts <= tc ) then
        t0 = tc
      else
        t0 = tc-0.005*(ts-tc)**2
      end if
   end if

   ! compute sea level pressure                                    

   slp=psfc*exp(2.0*gravity*terr/(gas_constant*(ts+t0)))

   if (trace_use) call da_trace_exit("da_tpq_to_slp")

end subroutine da_tpq_to_slp


