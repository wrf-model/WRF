real function da_unifva (kdum) 

   !--------------------------------------------------------------------
   ! Purpose: Minimal random number generator of Park and Miller with 
   ! Bays-Durham shuffle and added safeguards.
   ! Returns a uniform random deviate between 0.0. and 1.0 (exclusive 
   ! of the endpoint values). Call with kdum a negative integer to 
   ! initialize; thereafter, do not alter kdum between successive 
   ! deviates in sequence. rnmx should approximate the largest 
   ! floating value less than 1. 
   !
   ! See descripiton of function 'ran1', pg. 271.
   !--------------------------------------------------------------------
 
   implicit none
 
   integer, intent(inout) ::   KDUM

   integer JPIA,JPIM,JPIQ,JPIR,JPNTAB,JPNDIV
   real PPAM,PPEPS,PPRNMX

   parameter(JPIA=16807,JPIM=2147483647,JPIQ=127773,JPIR=2836, &
             JPNTAB=32,JPNDIV=1+(JPIM-1)/JPNTAB, &
             PPAM=1./JPIM,PPEPS=1.2E-07,PPRNMX=1.-PPEPS)

   integer JJ
   integer IJJ,IK
 
   integer NIV(JPNTAB),NIY
   save NIV,NIY
   DATA NIV /JPNTAB*0/, NIY /0/

   if (trace_use_frequent) call da_trace_entry("da_unifva")

   ! begin main
   ! ----------

   if ((KDUM.LE.0).OR.(NIY.EQ.0)) then
      KDUM = MAX(-KDUM , 1)

       do JJ = JPNTAB+8,1,-1
          IK   = KDUM/JPIQ
          KDUM = JPIA*(KDUM - IK*JPIQ) - JPIR*IK
 
          if (KDUM.lt.0) KDUM = KDUM + JPIM
          if (JJ.LE.JPNTAB) NIV(JJ) = KDUM
 
       end do

       NIY = NIV(1)
   end if
  
   IK   = KDUM/JPIQ
   KDUM = JPIA*(KDUM - IK*JPIQ) - JPIR*IK
     
   if (KDUM.LT.0) KDUM = KDUM + JPIM

   IJJ      = 1 + NIY/JPNDIV
   NIY      = NIV(IJJ)
   NIV(IJJ) = KDUM
   DA_UNifVA   = Min(PPAM*NIY , PPRNMX)

   if (trace_use_frequent) call da_trace_exit("da_unifva")

end function da_unifva


