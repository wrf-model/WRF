subroutine da_set_map_para

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real :: phictr, r, conv
   real :: cell, cell2, psx

   if (trace_use_dull) call da_trace_entry("da_set_map_para")

   conv = 180.0 / pi
                                                                  
   ! define psi1:
   if (map_projection==1 .or. map_projection==2) then
      if (phic.lt.0) then 
         psi1 = 90.0+truelat1_3dv
         psi1 = -psi1
      else
        psi1 = 90.0-truelat1_3dv
      end if          
   else
      psi1 = 0.0
   end if 

   psi1 = psi1/conv
         
   ! calculate r
   if (map_projection.ne.3) then
      psx = (pole-phic)/conv
      if (map_projection==1) then
         cell  = earth_radius*sin(psi1)/cone_factor  
         cell2 = (tan(psx/2.0))/(tan(psi1/2.0))
      end if 
      if (map_projection==2) then
         cell  = earth_radius*sin(psx)/cone_factor   
         cell2 = (1.0 + cos(psi1))/(1.0 + cos(psx)) 
      end if  
      r = cell*(cell2)**cone_factor 
      ycntr = -r
   end if  

   ! for mercator projection, the projection is true at lat at phi1
   if (map_projection==3) then
      c2     = earth_radius*cos(psi1) 
      phictr = phic/conv 
      cell   = cos(phictr)/(1.0+sin(phictr)) 
      ycntr  = - c2*log(cell)  
   end if 

   if (trace_use_dull) call da_trace_exit("da_set_map_para")

end subroutine da_set_map_para


