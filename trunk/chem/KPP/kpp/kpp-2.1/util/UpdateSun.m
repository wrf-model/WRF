function Update_SUN( )

global TIME SUN
 
   SunRise = 4.5;
   SunSet  = 19.5;
   Thour = TIME/3600.;
   Tlocal = Thour - floor(Thour/24)*24;

   if ( (Tlocal>=SunRise) & (Tlocal<=SunSet) ) 
     Ttmp = (2.0*Tlocal-SunRise-SunSet)/(SunSet-SunRise);
     if (Ttmp>0) 
       Ttmp =  Ttmp*Ttmp;
     else
       Ttmp = -Ttmp*Ttmp;
     end 
     SUN = ( 1.0 + cos(pi*Ttmp) )/2.0 ;
   else
     SUN = 0.0;
   end 

return % Update_SUN

