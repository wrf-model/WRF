subroutine da_tpq_to_slp_adj (T, Q, P, TERR, PSFC, T9, Q9, P9, PSFC9, SLP9)

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
   !              
   !-----------------------------------------------------------------------

   implicit none

   real,              intent(in)  :: TERR, PSFC
   real,              intent(in)  :: T(kms:kme), Q(kms:kme), P(kms:kme)
   real,              intent(out) :: T9(kms:kme), Q9(kms:kme), P9(kms:kme)
   real,              intent(in)  :: SLP9
   real,              intent(out) :: PSFC9

   ! real :: SLP

   integer              :: K, KLO, KHI
   real                 :: PL, T0, TS, XTERM,TLO, THI, TL
   real                 :: PL9,T09,TS9,TLO9,THI9,TL9,COEF1,COEF2
   real                 :: T08,TS8

   real, parameter      :: GAMMA  = 6.5E-3
   real, parameter      :: TC     = t_kelvin+17.5
   real, parameter      :: PCONST = 10000.0
   real, parameter      :: EPS    = 0.622

   if (trace_use) call da_trace_entry("da_tpq_to_slp_adj")
                                                                        
   ! sea level pressure                                            
                                                                          
   xterm=gamma* gas_constant / gravity
   
   ! compute pressure at pconst mb above surface (pl)              
                                                                          
   T9(:) = 0.0
   Q9(:) = 0.0
   P9(:) = 0.0

   if (terr <= 0.0) then
      psfc9=slp9
      if (trace_use) call da_trace_exit("da_tpq_to_slp_adj")
      return
   end if

   PL  = psfc - PCONST                                        
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
      write(unit=message(1),fmt='(A,F11.3,A)') &
         'Error finding pressure level ',PCONST,' Mb above the surface'
      write(unit=message(2),fmt='(A,F11.3,2X,A,F11.3)') &
         'PL=',PL,'  PSFC=',psfc
      call da_error(__FILE__,__LINE__,message(1:2))
   end if                                                         

   ! get temperature at pl (tl), extrapolate t at surface (ts)     
   ! and t at sea level (t0) with 6.5 k/km lapse rate              

   TLO   = t(KLO) * (EPS+q(KLO))/(EPS*(1.0+q(KLO)))
   THI   = t(KHI) * (EPS+q(KHI))/(EPS*(1.0+q(KHI)))
   COEF1 = ALOG(PL/p(KHI))
   COEF2 = ALOG(p(KLO)/p(KHI))
   TL    = THI-(THI-TLO)*COEF1/COEF2
   TS    = TL*(psfc/PL)**XTERM                           
   TS8   = TS
   T0    = TS +GAMMA*terr
   T08   = T0

   ! Correct sea level temperature if too hot                      

   if (t0 >= tc) then
      if (ts <= tc) then
         t0 = tc
      else
         t0 = tc-0.005*(ts-tc)**2
      end if
   end if
   
   psfc9 = slp9*EXP(2.0*gravity*TERR/(gas_constant*(TS+T0)))
   TS9 = -slp9*psfc*EXP(2.0*gravity*TERR/(gas_constant*(TS+T0)))*  &
       2.0*gravity*TERR/(gas_constant*(TS+T0)**2)
   T09 = -slp9*psfc*EXP(2.0*gravity*TERR/(gas_constant*(TS+T0)))*  &
       2.0*gravity*TERR/(gas_constant*(TS+T0)**2)

   if ( t08 >= tc ) then
      if ( ts8 > tc ) then
         ts9=ts9-0.01*(ts-tc)*t09
      end if
      t09=0.0
   end if

   TS9     = TS9+T09
   TL9     = TS9*(psfc/PL)**XTERM
   psfc9   = psfc9+TS9*XTERM*(TL/PL)*(psfc/PL)**(XTERM-1)      
   PL9     = -TS9*XTERM*(TL*psfc/(PL*PL))*(psfc/PL)**(XTERM-1)
   THI9    = (1.0-COEF1/COEF2)*TL9
   TLO9    = COEF1/COEF2*TL9
   PL9     = PL9-(THI-TLO)/(COEF2*PL)*TL9
   p9(KHI) = p9(KHI)+((THI-TLO)/(COEF2*p(KHI))*(1.0-COEF1/COEF2))*TL9
   p9(KLO) = p9(KLO)+(THI-TLO)*COEF1/(COEF2*COEF2*p(KLO))*TL9
   t9(KHI) = t9(KHI)+THI9* (EPS+q(KHI))/(EPS*(1.0+q(KHI)))
   q9(KHI) = q9(KHI)+THI9*t(KHI)*(1.0-EPS)/(EPS*(1.0+q(KHI))**2)
   t9(KLO) = t9(KLO)+TLO9* (EPS+q(KLO))/(EPS*(1.0+q(KLO)))
   q9(KLO) = q9(KLO)+TLO9*t(KLO)*(1.0-EPS)/(EPS*(1.0+q(KLO))**2)

   psfc9=psfc9+PL9

   if (trace_use) call da_trace_exit("da_tpq_to_slp_adj")

end subroutine da_tpq_to_slp_adj


