subroutine da_julian_day(NY,NM,ND,JD,METHOD) 

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! method = 1: input ---- ny, nm, nd.  output ---- jd                         
   ! method = 2: input ---- ny, jd.      output ---- nm, nd            
   !----------------------------------------------------------------------- 
    
   implicit none

   integer,  intent(in)    :: METHOD, NY
   integer,  intent(inout) :: NM, ND, JD

   integer                 :: LOOP
   integer, dimension(12)  :: MDAY = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   if (trace_use_dull) call da_trace_entry("da_julian_day")

   if (mod(ny,4) == 0) then
      mday(2)=29      
      if (mod(ny,100) == 0) then
         mday(2)=28
         if (mod(ny,400) == 0) then
            mday(2)=29
         end if
      end if
   end if

   if (method == 1) then                  
      jd=nd
      juday: do loop=1,nm-1                  
         jd=jd+mday(loop)                
      end do juday                           
   else if (method == 2) then             
      nm=1                               
      nd=jd
      do loop=1,11                    
         if (nd <= mday(loop)) exit

         nd=nd-mday(loop)     
         nm=nm+1                      
      end do
   end if   

   if (trace_use_dull) call da_trace_exit("da_julian_day")                             

end subroutine da_julian_day


