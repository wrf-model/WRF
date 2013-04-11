subroutine da_xyll_default(XX,YY,XLAT,XLON)

   !-----------------------------------------------------------------------
   ! Purpose: calculates the latitudes and longitudes from the
   !                  (x,y) location (dot) in the mesoscale grids.
   ! on entry     :   
   ! x            : the coordinate in x (j)-direction.
   ! y            : the coordinate in y (i)-direction.
   !
   ! on exit      :                      
   ! xlat         : latitudes 
   ! xlon         : longitudes 
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: XX, YY
   real, intent(out) :: XLAT,XLON
        
   real              :: flp, flpp, r, cell, cel1, cel2
   real              :: rcone_factor, psx, conv
   real              :: cntri, cntrj, x, y, xcntr

   if (trace_use) call da_trace_entry("da_xyll_default")
   
   conv = 180.0 / pi

   ! seperate treatment for global fields
   if (fg_format == fg_format_kma_global) then
      xlat = yy * 180.0 /(coarse_jy-1)  -  90.0    
      xlon = xx * 360.0 /(coarse_ix-1)  - 180.0    
      return
   end if

   cntri = real(coarse_ix+1)/2.0
   cntrj = real(coarse_jy+1)/2.0

   xcntr = 0.0

   ! calculate x and y positions of grid

   x = (xx - 0.5)*dsm/coarse_ds + start_x
   y = (yy - 0.5)*dsm/coarse_ds + start_y
   x = xcntr + (x-cntri)*coarse_ds
   y = ycntr + (y-cntrj)*coarse_ds

   ! now calculate lat and lon of this point

   if (map_projection.ne.3) then
      if(y.eq.0.0) then      
         if (x.ge.0.0) flp =  90.0/conv 
         if (x.lt.0.0) flp = -90.0/conv
      else
         if (phic.lt.0.0)then
            flp = atan2(x,y)   
         else
            flp = atan2(x,-y) 
         end if
      end if 
      flpp = (flp/cone_factor)*conv+xlonc  
      if (flpp.lt.-180.0) flpp = flpp + 360.0    
      if (flpp.gt.180.0)  flpp = flpp - 360.0  
      xlon = flpp 
      ! now solve for latitude
      r = sqrt(x*x+y*y)  
      if (phic.lt.0.0) r = -r  
      if (map_projection.eq.1) then   
         cell = (r*cone_factor)/(earth_radius*sin(psi1))    
         rcone_factor  = 1.0/cone_factor   
         cel1 = tan(psi1/2.0)*(cell)**rcone_factor    
      end if 
      if (map_projection.eq.2) then
         cell = r/earth_radius        
         cel1 = cell/(1.0+cos(psi1))  
      end if 
      cel2 = atan(cel1)    
      psx  = 2.0*cel2*conv
      xlat = pole-psx 
   end if   
   ! calculations for mercator lat,lon    
   if (map_projection.eq.3) then   
      xlon = xlonc + ((x-xcntr)/c2)*conv 
      if (xlon.lt.-180.0) xlon = xlon + 360.0
      if (xlon.gt.180.0)  xlon = xlon - 360.0
      cell = exp(y/c2)  
      xlat = 2.0*(conv*atan(cell))-90.0 
   end if  

   if (trace_use) call da_trace_exit("da_xyll_default")

end subroutine da_xyll_default


