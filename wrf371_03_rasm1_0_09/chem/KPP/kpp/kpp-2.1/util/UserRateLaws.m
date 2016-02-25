%  User-defined Rate Law functions
%  Note: insert this file at the end of Update_RCONST

%---  Arrhenius
   function [rate] =  ARR( A0,B0,C0 )
      global TEMP CFACTOR
      rate =  (A0) * exp(-(B0)/TEMP) * (TEMP/300.0)^(C0) ;            
   return %  ARR        

%--- Simplified Arrhenius, with two arguments
%--- Note: The argument B0 has a changed sign when compared to ARR
   function [rate] =  ARR2( A0,B0 )
      global TEMP CFACTOR
      rate =  (A0) * exp( (B0)/TEMP ) ;             
   return %  ARR2          

   function [rate] =  EP2(A0,C0,A2,C2,A3,C3)
      global TEMP CFACTOR                       
      K0 = (A0) * exp(-C0/TEMP);
      K2 = (A2) * exp(-C2/TEMP);
      K3 = (A3) * exp(-C3/TEMP);
      K3 = K3*CFACTOR*1.0e+6;
      rate = K0 + K3/(1.0+K3/K2) ;       
   return %  EP2

   function [rate] =  EP3(A1,C1,A2,C2) 
      global TEMP CFACTOR               
      K1 = (A1) * exp(-(C1)/TEMP);
      K2 = (A2) * exp(-(C2)/TEMP);
      rate = K1 + K2*(1.0e+6*CFACTOR);      
   return %  EP3 

   function [rate] =  FALL ( A0,B0,C0,A1,B1,C1,CF)
      global TEMP CFACTOR                      
      K0 = A0 * exp(-B0/TEMP)* (TEMP/300.0)^(C0);
      K1 = A1 * exp(-B1/TEMP)* (TEMP/300.0)^(C1);
      K0 = K0*CFACTOR*1.0e+6;
      K1 = K0/K1;
      rate = (K0/(1.0+K1))*(CF)^(1.0/(1.0+(log(K1))^2));        
   return %  FALL

